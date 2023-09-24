// Generated by cpp11: do not edit by hand
// clang-format off


#include "cpp11/declarations.hpp"
#include <R_ext/Visibility.h>

// lifetab.cpp
writable::logicals is_ax_le_nx(doubles ax, strings age_group_categ);
extern "C" SEXP _poputils_is_ax_le_nx(SEXP ax, SEXP age_group_categ) {
  BEGIN_CPP11
    return cpp11::as_sexp(is_ax_le_nx(cpp11::as_cpp<cpp11::decay_t<doubles>>(ax), cpp11::as_cpp<cpp11::decay_t<strings>>(age_group_categ)));
  END_CPP11
}
// lifetab.cpp
writable::doubles_matrix<> Lx_to_ex(cpp11::doubles_matrix<> Lx);
extern "C" SEXP _poputils_Lx_to_ex(SEXP Lx) {
  BEGIN_CPP11
    return cpp11::as_sexp(Lx_to_ex(cpp11::as_cpp<cpp11::decay_t<cpp11::doubles_matrix<>>>(Lx)));
  END_CPP11
}
// lifetab.cpp
writable::doubles_matrix<> lx_to_dx(cpp11::doubles_matrix<> lx);
extern "C" SEXP _poputils_lx_to_dx(SEXP lx) {
  BEGIN_CPP11
    return cpp11::as_sexp(lx_to_dx(cpp11::as_cpp<cpp11::decay_t<cpp11::doubles_matrix<>>>(lx)));
  END_CPP11
}
// lifetab.cpp
writable::doubles_matrix<> lx_to_qx(cpp11::doubles_matrix<> lx);
extern "C" SEXP _poputils_lx_to_qx(SEXP lx) {
  BEGIN_CPP11
    return cpp11::as_sexp(lx_to_qx(cpp11::as_cpp<cpp11::decay_t<cpp11::doubles_matrix<>>>(lx)));
  END_CPP11
}
// lifetab.cpp
writable::doubles_matrix<> mx_to_ex(cpp11::doubles_matrix<> mx, strings age_group_categ, strings sex, doubles ax, strings methods);
extern "C" SEXP _poputils_mx_to_ex(SEXP mx, SEXP age_group_categ, SEXP sex, SEXP ax, SEXP methods) {
  BEGIN_CPP11
    return cpp11::as_sexp(mx_to_ex(cpp11::as_cpp<cpp11::decay_t<cpp11::doubles_matrix<>>>(mx), cpp11::as_cpp<cpp11::decay_t<strings>>(age_group_categ), cpp11::as_cpp<cpp11::decay_t<strings>>(sex), cpp11::as_cpp<cpp11::decay_t<doubles>>(ax), cpp11::as_cpp<cpp11::decay_t<strings>>(methods)));
  END_CPP11
}
// lifetab.cpp
writable::doubles_matrix<> mx_to_lx(cpp11::doubles_matrix<> mx, strings age_group_categ, strings sex, doubles ax, strings methods);
extern "C" SEXP _poputils_mx_to_lx(SEXP mx, SEXP age_group_categ, SEXP sex, SEXP ax, SEXP methods) {
  BEGIN_CPP11
    return cpp11::as_sexp(mx_to_lx(cpp11::as_cpp<cpp11::decay_t<cpp11::doubles_matrix<>>>(mx), cpp11::as_cpp<cpp11::decay_t<strings>>(age_group_categ), cpp11::as_cpp<cpp11::decay_t<strings>>(sex), cpp11::as_cpp<cpp11::decay_t<doubles>>(ax), cpp11::as_cpp<cpp11::decay_t<strings>>(methods)));
  END_CPP11
}
// lifetab.cpp
writable::doubles_matrix<> mx_to_Lx(cpp11::doubles_matrix<> mx, strings age_group_categ, strings sex, doubles ax, strings methods);
extern "C" SEXP _poputils_mx_to_Lx(SEXP mx, SEXP age_group_categ, SEXP sex, SEXP ax, SEXP methods) {
  BEGIN_CPP11
    return cpp11::as_sexp(mx_to_Lx(cpp11::as_cpp<cpp11::decay_t<cpp11::doubles_matrix<>>>(mx), cpp11::as_cpp<cpp11::decay_t<strings>>(age_group_categ), cpp11::as_cpp<cpp11::decay_t<strings>>(sex), cpp11::as_cpp<cpp11::decay_t<doubles>>(ax), cpp11::as_cpp<cpp11::decay_t<strings>>(methods)));
  END_CPP11
}
// lifetab.cpp
writable::doubles_matrix<> qx_to_lx(cpp11::doubles_matrix<> qx);
extern "C" SEXP _poputils_qx_to_lx(SEXP qx) {
  BEGIN_CPP11
    return cpp11::as_sexp(qx_to_lx(cpp11::as_cpp<cpp11::decay_t<cpp11::doubles_matrix<>>>(qx)));
  END_CPP11
}

extern "C" {
static const R_CallMethodDef CallEntries[] = {
    {"_poputils_Lx_to_ex",    (DL_FUNC) &_poputils_Lx_to_ex,    1},
    {"_poputils_is_ax_le_nx", (DL_FUNC) &_poputils_is_ax_le_nx, 2},
    {"_poputils_lx_to_dx",    (DL_FUNC) &_poputils_lx_to_dx,    1},
    {"_poputils_lx_to_qx",    (DL_FUNC) &_poputils_lx_to_qx,    1},
    {"_poputils_mx_to_Lx",    (DL_FUNC) &_poputils_mx_to_Lx,    5},
    {"_poputils_mx_to_ex",    (DL_FUNC) &_poputils_mx_to_ex,    5},
    {"_poputils_mx_to_lx",    (DL_FUNC) &_poputils_mx_to_lx,    5},
    {"_poputils_qx_to_lx",    (DL_FUNC) &_poputils_qx_to_lx,    1},
    {NULL, NULL, 0}
};
}

extern "C" attribute_visible void R_init_poputils(DllInfo* dll){
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
