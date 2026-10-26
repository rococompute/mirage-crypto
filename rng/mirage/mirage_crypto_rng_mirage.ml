(*
 * Copyright (c) 2014 Hannes Mehnert
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014-2016 David Kaloper Meršinjak
 * Copyright (c) 2015 Citrix Systems Inc
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * * Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 *
 * * Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

let src = Logs.Src.create "mirage-crypto-rng-mirage" ~doc:"Mirage crypto RNG mirage"
module Log = (val Logs.src_log src : Logs.LOG)

open Mirage_crypto_rng

let rdrand_task delta =
  match Entropy.cpu_rng with
  | Error `Not_supported -> ()
  | Ok cpu_rng ->
    let open Lwt.Infix in
    let rdrand = cpu_rng None in
    Lwt.async (fun () ->
        let rec one () =
          rdrand ();
          Mirage_sleep.ns delta >>=
          one
        in
        one ())

(* Random bytes from the host, via solo5_random() on Solo5 tenders that
   offer it. On Apple silicon there is no CPU RNG and the cycle counter runs
   at 24 MHz, so this is the only good seed source. *)
external solo5_random : bytes -> int -> int -> bool = "mc_solo5_random"
  [@@noalloc]

let host_chunk = 32

(* solo5_random() serves at most 4096 bytes per call. *)
let host_random n =
  let buf = Bytes.create n in
  let rec fill off =
    if off >= n then true
    else
      let len = min 4096 (n - off) in
      solo5_random buf off len && fill (off + len)
  in
  if fill 0 then Some (Bytes.unsafe_to_string buf) else None

let host_seed id data =
  let hdr = Bytes.create 2 in
  Bytes.set_uint8 hdr 0 id;
  Bytes.set_uint8 hdr 1 (String.length data);
  Bytes.unsafe_to_string hdr ^ data

(* Feeds host_chunk host bytes into each pool, with one hypercall for all of
   them. *)
let feed_host g source =
  let pools = pools g in
  match host_random (pools * host_chunk) with
  | None -> ()
  | Some data ->
    let i = ref 0 in
    Entropy.feed_pools g source (fun () ->
        let chunk = String.sub data (!i * host_chunk) host_chunk in
        incr i;
        Ok chunk)

let host_task g source delta =
  let open Lwt.Infix in
  Lwt.async (fun () ->
      let rec one () =
        feed_host g source;
        Mirage_sleep.ns delta >>=
        one
      in
      one ())

let bootstrap_functions () =
  Entropy.[ bootstrap ; bootstrap ; whirlwind_bootstrap ; bootstrap ]

let running = ref false

let initialize (type a) ?g ?(sleep = Duration.of_sec 1) (rng : a generator) =
  if !running then
    Lwt.fail_with "entropy collection already running"
  else begin
    entropy_test ();
    (try
       let _ = default_generator () in
       Log.warn (fun m -> m "Mirage_crypto_rng.default_generator has already \
                             been set, check that this call is intentional");
     with
       No_default_generator -> ());
    running := true;
    let host = host_random host_chunk in
    let seed =
      let rest = List.mapi (fun i f -> f i) (bootstrap_functions ()) in
      let host =
        match host with
        | None -> []
        | Some data -> [ host_seed (List.length rest) data ]
      in
      String.concat "" (host @ rest)
    in
    let rng = create ?g ~seed ~time:Mirage_mtime.elapsed_ns rng in
    set_default_generator rng;
    if Option.is_some host then begin
      let source = Entropy.register_source "solo5-host" in
      host_task (Some rng) source sleep
    end else
      Log.info (fun m -> m "no host entropy (solo5_random unavailable), \
                            seeding from CPU RNG or whirlwind only");
    rdrand_task sleep;
    Mirage_runtime.at_enter_iter (Entropy.timer_accumulator None);
    Lwt.return_unit
  end
