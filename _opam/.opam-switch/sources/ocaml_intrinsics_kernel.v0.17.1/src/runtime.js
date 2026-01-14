//Provides: caml_csel_value
function caml_csel_value(v_cond, v_true, v_false) {
  if (v_cond)
    return v_true;
  else
    return v_false;
}

//Provides: caml_sse2_float64_min_bytecode
function caml_sse2_float64_min_bytecode(x, y) {
  return x < y ? x : y;
}

//Provides: caml_sse2_float64_max_bytecode
function caml_sse2_float64_max_bytecode(x, y) {
  return x > y ? x : y;
}
