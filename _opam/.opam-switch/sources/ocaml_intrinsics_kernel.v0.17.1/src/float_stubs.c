#include <stdint.h>
#include <assert.h>

#include <caml/alloc.h>
#include <caml/mlvalues.h>

#if defined(__SSE2__) || defined(_MSC_VER)

#ifdef _MSC_VER
#include <intrin.h>
#else // _MSC_VER
#include <emmintrin.h>
#endif // _MSC_VER

double caml_sse2_float64_min(double x, double y)
{
  return _mm_cvtsd_f64(_mm_min_sd(_mm_set_sd(x), _mm_set_sd(y)));
}

double caml_sse2_float64_max(double x, double y)
{
  return _mm_cvtsd_f64(_mm_max_sd(_mm_set_sd(x), _mm_set_sd(y)));
}

#else // __SSE2__ || _MSC_VER

#include <math.h>

#if defined(__GNUC__)
__attribute__((optimize("no-math-errno")))
#endif

double caml_sse2_float64_min(double x, double y) {
  return x < y ? x : y;
}

double caml_sse2_float64_max(double x, double y) {
  return x > y ? x : y;
}

#endif // __SSE2__

CAMLprim value caml_sse2_float64_min_bytecode(value x, value y)
{
  return caml_copy_double(caml_sse2_float64_min(Double_val(x), Double_val(y)));
}

CAMLprim value caml_sse2_float64_max_bytecode(value x, value y)
{
  return caml_copy_double(caml_sse2_float64_max(Double_val(x), Double_val(y)));
}

