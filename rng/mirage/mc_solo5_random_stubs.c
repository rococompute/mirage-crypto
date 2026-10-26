/*
 * Host random bytes from the Solo5 tender, when there is one that offers
 * them. Everywhere else (Unix, older Solo5 headers, tenders without the
 * feature) this reports failure and the caller uses its other sources.
 */

#include <stdint.h>
#include <caml/mlvalues.h>

#if defined(__ocaml_solo5__)
#include <solo5.h>
#endif

CAMLprim value mc_solo5_random(value buf, value off, value len)
{
#if defined(__ocaml_solo5__) && defined(SOLO5_HAVE_RANDOM)
  return Val_bool(solo5_random((uint8_t *)Bytes_val(buf) + Long_val(off),
                               Long_val(len)) == SOLO5_R_OK);
#else
  (void)buf; (void)off; (void)len;
  return Val_false;
#endif
}
