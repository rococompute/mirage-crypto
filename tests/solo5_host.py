#!/usr/bin/env python3
# Exercise the real Mirage RNG module and C FFI with a fake Solo5 entropy
# provider and small adapters for the RNG/timer APIs. This checks host-source
# wiring, chunking and failure state changes; it does not test Fortuna or HVF.
# Run with ocamlfind packages lwt and logs available (e.g. opam exec -- python3 ...).
import os, pathlib, subprocess, tempfile
root=pathlib.Path(os.environ.get('CRYPTO_SOURCE', pathlib.Path(__file__).resolve().parents[1]))
files={
 'mirage_crypto_rng.ml': '''
type 'a generator = unit
exception No_default_generator
let last_seed = ref ""
let pool_count = ref 32
let default_generator () = raise No_default_generator
let entropy_test () = ()
let pools _ = !pool_count
let create ?g:_ ~seed ~time:_ _ = last_seed := seed; ()
let set_default_generator _ = ()
module Entropy = struct
  let registered : (int * string) list ref = ref []
  let feeds : ((int * string) * string) list ref = ref []
  let cpu_rng : ((unit option -> unit -> unit), [ `Not_supported ]) result = Error `Not_supported
  let bootstrap id = String.make 1 (Char.chr id)
  let whirlwind_bootstrap = bootstrap
  let register_source name = let source = (7 + List.length !registered, name) in registered := source :: !registered; source
  let id = fst
  let feed_pools _ source f = for _ = 1 to !pool_count do match f () with Ok data -> feeds := (source, data) :: !feeds | Error _ -> assert false done
  let timer_accumulator _ = fun () -> ()
end
''',
 'mirage_sleep.ml': '''
let pending : unit Lwt.u option ref = ref None
let ns _ = let promise, resolver = Lwt.wait () in pending := Some resolver; promise
let step () = match !pending with None -> assert false | Some resolver -> pending := None; Lwt.wakeup resolver ()
''',
 'duration.ml': 'let of_sec n = Int64.mul (Int64.of_int n) 1000000000L\n',
 'mirage_mtime.ml': 'let elapsed_ns () = 0L\n',
 'mirage_runtime.ml': 'let at_enter_iter _ = ()\n',
 'solo5.h': '''
#include <stddef.h>
#define SOLO5_HAVE_RANDOM 1
#define SOLO5_R_OK 0
int solo5_random(void *, size_t);
''',
 'fake.c': '''
#include <string.h>
#include <caml/mlvalues.h>
static int failing, calls;
int solo5_random(void *p, size_t n) { calls++; if(failing) return 1; memset(p, 0xa5, n); return 0; }
CAMLprim value fake_fail(value f) { failing = Bool_val(f); return Val_unit; }
CAMLprim value fake_calls(value _) { return Val_int(calls); }
''',
 'driver.ml': '''
external fail : bool -> unit = "fake_fail"
external calls : unit -> int = "fake_calls"
module R = Mirage_crypto_rng
module H = Mirage_crypto_rng_mirage
let messages = ref []
let reporter = { Logs.report = fun _src _level ~over k msgf -> msgf @@ fun ?header:_ ?tags:_ fmt -> Format.kasprintf (fun msg -> messages := msg :: !messages; over (); k ()) fmt }
let () =
 Logs.set_level (Some Logs.Debug); Logs.set_reporter reporter;
 if Array.length Sys.argv > 1 then begin
  fail true; ignore(H.initialize ());
  assert (!(R.Entropy.registered) = []);
  assert(List.exists (fun m -> String.starts_with ~prefix:"no host entropy" m) !messages);
  print_endline "boot fallback passed"
 end else begin
  let before = calls () in
  assert(Option.map String.length (H.host_random 10000) = Some 10000);
  assert(calls () - before = 3);
  ignore(H.initialize ());
  assert(List.mem (7, "solo5-host") !(R.Entropy.registered));
  assert(Char.code !(R.last_seed).[0] = 7);
  assert(List.length !(R.Entropy.feeds) = 32);
  fail true; Mirage_sleep.step (); Mirage_sleep.step ();
  assert(List.length !messages = 1);
  assert(List.hd !messages = "solo5-host entropy unavailable; retrying");
  fail false; Mirage_sleep.step ();
  assert(List.length !messages = 2);
  assert(List.hd !messages = "solo5-host entropy recovered");
  R.pool_count := 160;
  let before = calls () in
  ignore(H.feed_host None (7, "solo5-host"));
  assert(calls () - before = 2);
  print_endline "4096-byte chunking, registered host seed, all pools, failure/recovery logging passed"
 end
'''
}
with tempfile.TemporaryDirectory(prefix='crypto-host-') as d:
 p=pathlib.Path(d)
 for name,text in files.items(): (p/name).write_text(text)
 for name in ['mirage_crypto_rng_mirage.ml','mc_solo5_random_stubs.c']:
  (p/name).write_bytes((root/'rng/mirage'/name).read_bytes())
 def run(*args):
  print('+',*args,flush=True);subprocess.run(args,cwd=d,check=True)
 run('ocamlopt','-c','-ccopt','-D__ocaml_solo5__','-ccopt','-I.','mc_solo5_random_stubs.c')
 run('ocamlopt','-c','fake.c')
 run('ocamlfind','ocamlopt','-linkpkg','-package','lwt,logs','duration.ml','mirage_crypto_rng.ml','mirage_sleep.ml','mirage_mtime.ml','mirage_runtime.ml','mirage_crypto_rng_mirage.ml','driver.ml','mc_solo5_random_stubs.o','fake.o','-o','driver')
 run('./driver');run('./driver','fallback')
