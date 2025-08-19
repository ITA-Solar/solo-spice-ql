#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <idl_export.h>

static void bailout(char *msg)
{
  IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_LONGJMP, msg);
}

static void info(char *msg)
{
  IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, msg);
}

void assert_numeric(const char *description, IDL_VPTR arg)
{
  char msg[256];
  if (arg->type != IDL_TYP_DOUBLE && arg->type != IDL_TYP_FLOAT && arg->type != IDL_TYP_INT &&
      arg->type != IDL_TYP_UINT && arg->type != IDL_TYP_LONG && arg->type != IDL_TYP_ULONG) {
    snprintf(msg, sizeof(msg), "%s must be numeric", description);
    bailout(msg);
  }
}

void check_numeric_array_params(int argc, IDL_VPTR Argv[])
{
  IDL_ENSURE_ARRAY(Argv[0]);
  IDL_ENSURE_ARRAY(Argv[1]);
  if (Argv[1]->value.arr->n_dim != 1) {
    bailout("A (coefficients) must be a 1-dimensional array");
  }
  assert_numeric("X (1st arg)", Argv[0]);
  assert_numeric("A (coefficients)", Argv[1]);
}

void make_arr_0_from_template(IDL_VPTR x_vptr, IDL_VPTR dest)
{
  IDL_VPTR tmp;
  // First temporary then copy it where caller wants
  IDL_VarMakeTempFromTemplate(x_vptr, x_vptr->type, 0 /*not struct*/, &tmp, TRUE);
  IDL_VarCopy(tmp, dest);
}

void make_pder_array(IDL_VPTR x_vptr, IDL_VPTR a_vptr, IDL_VPTR pder)
{
  IDL_MEMINT pder_dim[2];
  if (pder) {
    pder_dim[0] = x_vptr->value.arr->n_elts;
    pder_dim[1] = a_vptr->value.arr->n_elts;
    IDL_VPTR tmp;
    IDL_MakeTempArray(IDL_TYP_DOUBLE, 2, pder_dim, IDL_ARR_INI_NOP, &tmp);
    IDL_VarCopy(tmp, pder);
  }
}

double *make_a_param_vector(IDL_VPTR vptr, IDL_LONG64 n_elts)
{
  IDL_VPTR tmp;
  IDL_MakeTempArray(IDL_TYP_DOUBLE, 1, &n_elts, IDL_ARR_INI_NOP, &tmp);
  IDL_VarCopy(tmp, vptr);
  return (double *) vptr->value.arr->data;
}

// ****************************************************************************************************
// ******************************************* COMP_POLY **********************************************
// ****************************************************************************************************

/*; Use         : COMP_POLY,X,A,F [,PDER]*/
static void COMP_POLY(int argc, IDL_VPTR Argv[], char *argk)
{
  check_numeric_array_params(argc, Argv);
  IDL_VPTR x_vptr = IDL_CvtDbl(1, Argv);
  IDL_VPTR a_vptr = IDL_CvtDbl(1, Argv + 1);

  IDL_VPTR f_vptr = Argv[2]; /* Output array */
  IDL_VPTR pder_vptr = argc > 3 ? Argv[3] : NULL; // Partial derivatives, optional

  make_arr_0_from_template(x_vptr, f_vptr);

  int degree = a_vptr->value.arr->n_elts - 1;
  double *x = (void *) x_vptr->value.arr->data;
  double *a = (void *) a_vptr->value.arr->data;
  double *f = (void *) f_vptr->value.arr->data;
  double *pder = NULL;

  if (pder_vptr) {
    make_pder_array(x_vptr, a_vptr, pder_vptr);
    pder = (void *) pder_vptr->value.arr->data;
  }

  IDL_MEMINT Nx = x_vptr->value.arr->n_elts;
  for (IDL_MEMINT ix = 0; ix < x_vptr->value.arr->n_elts; ix++) {
    f[ix] = 0.0;
    for (IDL_MEMINT aix = 0; aix <= degree; aix++) {
      f[ix] += a[aix] * pow(x[ix], aix);
      if (pder_vptr) {
        // Partial derivative of f with respect to coefficient a[aix]:
        pder[ix + aix * Nx] = pow(x[ix], aix);
      }
    }
  }

  // These might be temp. due to type conversion
  if (x_vptr != Argv[0]) {
    IDL_DELTMP(x_vptr);
  }
  if (a_vptr != Argv[1]) {
    IDL_DELTMP(a_vptr);
  }
}

// ****************************************************************************************************
// ******************************************* COMP_GAUSS *********************************************
// ****************************************************************************************************

/*; Use         : COMP_GAUSS,X,A,F [,PDER]*/
static void COMP_GAUSS(int argc, IDL_VPTR Argv[], char *argk)
{
  char msg[256];
  check_numeric_array_params(argc, Argv);

  IDL_VPTR x_vptr = IDL_CvtDbl(1, Argv);
  IDL_VPTR a_vptr = IDL_CvtDbl(1, Argv + 1);
  IDL_VPTR f_vptr = Argv[2]; /* Output array */
  IDL_VPTR pder_vptr = argc > 3 ? Argv[3] : NULL; // Partial derivatives, optional

  make_arr_0_from_template(x_vptr, f_vptr);

  double *a = (void *) a_vptr->value.arr->data;
  double *f = (void *) f_vptr->value.arr->data;
  double *x = (void *) x_vptr->value.arr->data;
  double *pder = NULL;

  if (pder_vptr) {
    make_pder_array(x_vptr, a_vptr, pder_vptr);
    pder = (void *) pder_vptr->value.arr->data;
  }

  IDL_MEMINT Nx = x_vptr->value.arr->n_elts;
  for (IDL_MEMINT ix = 0; ix < x_vptr->value.arr->n_elts; ix++) {
    double z = (x[ix] - a[1]) / a[2];
    double z2 = z * z;
    double kern = exp(-z2 * 0.5);
    kern = (z2 < 1000.0) ? kern : 0.0; // Avoid exp overflow
    f[ix] = a[0] * kern;

    if (pder_vptr) {
      pder[ix + 0 * Nx] = kern;
      pder[ix + 1 * Nx] = f[ix] * z / a[2];
      pder[ix + 2 * Nx] = pder[ix + 1 * Nx] * z;
    }
  }
  // These might be temp. due to type conversion
  if (x_vptr != Argv[0]) {
    IDL_DELTMP(x_vptr);
  }
  if (a_vptr != Argv[1]) {
    IDL_DELTMP(a_vptr);
  }
}

/* IDL code (-ish)
  z = (x-a(1))/a(2)
  z2 = z*z
  ix = where(z2 LT 1000.0) ;; Exp(-1000) == 0 unless quadruple precision
  kern = exp(-z2(ix)*0.5)

  f(ix) = a(0)*kern

  pder(ix,0) = kern
  pder(ix,1) = f(ix) * z(ix)/a(2)
  pder(ix,2) = pder(ix,1) * z(ix)
  help,pder
*/

/* Testing:
x = [500.000, 500.200, 500.400, 500.600, 500.800, 501.000, 501.200, 501.400, 501.600, 501.800, 502.000, 502.200, 502.400, 502.600, 502.800, 503.000, 503.200, 503.400, 503.600, 503.800] 
a = [45., 502., 0.4]
comp_gauss,x,a,f,pder
window,0
plot,x,f
oplot,x,pder[*,0]*10
oplot,x,pder[*,1]/10.
oplot,x,pder[*,2]/20.
*/

// Non-IDL-callable, used to set up calls to COMP_GAUSS (which is callable)
// *a points to first param (offset has been applied)
// *pder points to first "row" of pders for for our params (offset has been applied)
static void cf_gauss(IDL_VPTR x_vptr, double *a, IDL_VPTR f_vptr, double *pder)
{
  int argc = pder ? 4 : 3; // 3 or 4 args to COMP_GAUSS
  double *f = (void *) f_vptr->value.arr->data;
  IDL_VPTR comp_f_vptr = IDL_Gettmp(); // For receiving result f from component
  IDL_VPTR comp_pder_vptr = pder ? IDL_Gettmp() : NULL; // For receiving pder from component
  IDL_VPTR comp_a_vptr = IDL_Gettmp(); // For gauss params

  double *comp_a = make_a_param_vector(comp_a_vptr, 3); // a0, a1, a2
  comp_a[0] = a[0]; // a0 is the height
  comp_a[1] = a[1]; // a1 is the center
  comp_a[2] = a[2]; // a2 is the width

  IDL_VPTR comp_args[4]; // For sending args to component
  comp_args[0] = x_vptr; // Input array
  comp_args[1] = comp_a_vptr; // Coefficients
  comp_args[2] = comp_f_vptr; // Output
  comp_args[3] = comp_pder_vptr; // Partial derivatives, optional
  COMP_GAUSS(argc, comp_args, NULL); // Call COMP_GAUSS with 3 or 4 args

  // Copy component result:
  double *comp_f = (void *) comp_f_vptr->value.arr->data;
  IDL_MEMINT Nx = x_vptr->value.arr->n_elts;
  for (int i = 0; i < Nx; i++) {
    f[i] += comp_f[i];
  }

  // Copy partial derivatives from comp_gauss to our pder
  // Our pder = array[Nx,  Na] and all pder[*,i] are consecutive,
  // i.e....
  //   comp_pder = array[Nx, cNa] and cNa = 3 (comp. has 3 parms)
  if (pder) {
    double *comp_pder = (void *) comp_pder_vptr->value.arr->data;
    for (int param = 0; param < 3; param++) {
      for (int ix = 0; ix < Nx; ix++) {
        pder[ix + param * Nx] = comp_pder[ix + param * Nx];
      }
    }
  }

  IDL_DELTMP(comp_a_vptr);
  IDL_DELTMP(comp_f_vptr);
  if (comp_pder_vptr) {
    IDL_DELTMP(comp_pder_vptr);
  }
}

// Non-IDL-callable, used to set up calls to COMP_POLY (which is callable)
// *a points to first param (offset has been applied)
// *pder points to first "row" of pders for for our params (offset has been applied)
static void cf_poly0(IDL_VPTR x_vptr, double *a, IDL_VPTR f_vptr, double *pder)
{
  int argc = pder ? 4 : 3; // 3 or 4 args to COMP_POLY
  double *f = (void *) f_vptr->value.arr->data;
  IDL_VPTR comp_f_vptr = IDL_Gettmp(); // For receiving result f from component
  IDL_VPTR comp_pder_vptr = pder ? IDL_Gettmp() : NULL; // For receiving pder from component
  IDL_VPTR comp_a_vptr = IDL_Gettmp(); // For gauss params

  double *comp_a = make_a_param_vector(comp_a_vptr, 1);
  // Only zero-order polynomials so far
  comp_a[0] = a[0];
  //
  //

  IDL_VPTR comp_args[4]; // For sending args to component
  comp_args[0] = x_vptr; // Input array
  comp_args[1] = comp_a_vptr; // Coefficients for polynomial
  comp_args[2] = comp_f_vptr; // Output array for polynomial
  comp_args[3] = comp_pder_vptr; // Partial derivatives for polynomial, optional
  COMP_POLY(argc, comp_args, NULL); // Call COMP_POLY with 3 or 4 args

  // Copy component result:
  double *comp_f = (void *) comp_f_vptr->value.arr->data;
  IDL_MEMINT Nx = x_vptr->value.arr->n_elts;
  for (int i = 0; i < Nx; i++) {
    f[i] += comp_f[i];
  }

  // Copy partial derivatives from comp_gauss to our pder
  // Our pder = array[Nx,  Na] and all pder[*,i] are consecutive,
  // i.e....
  //   comp_pder = array[Nx, cNa] and cNa = 3 (comp. has 3 parms)
  if (pder) {
    double *comp_pder = (void *) comp_pder_vptr->value.arr->data;
    for (int param = 0; param < 1; param++) {
      for (int ix = 0; ix < Nx; ix++) {
        pder[ix + param * Nx] = comp_pder[ix + param * Nx];
      }
    }
  }

  IDL_DELTMP(comp_a_vptr);
  IDL_DELTMP(comp_f_vptr);
  if (comp_pder_vptr) {
    IDL_DELTMP(comp_pder_vptr);
  }
}

// Non-IDL-callable, used to set up calls to COMP_POLY (which is callable)
// *a points to first param (offset has been applied)
// *pder points to first "row" of pders for for our params (offset has been applied)
static void cf_polyN(IDL_VPTR x_vptr, double *a, IDL_VPTR f_vptr, double *pder, int degree)
{
  double *f = (void *) f_vptr->value.arr->data;
  IDL_VPTR comp_f_vptr = IDL_Gettmp(); // For receiving result f from component
  IDL_VPTR comp_pder_vptr = pder ? IDL_Gettmp() : NULL; // For receiving pder from component
  IDL_VPTR comp_a_vptr = IDL_Gettmp(); // For gauss params

  double *comp_a = make_a_param_vector(comp_a_vptr, degree + 1);

  for (int i = 0; i <= degree; i++) {
    comp_a[i] = a[i]; // Copy coefficients for polynomial
  }

  IDL_VPTR comp_args[4]; // For sending args to component
  comp_args[0] = x_vptr; // Input array
  comp_args[1] = comp_a_vptr; // Coefficients for polynomial
  comp_args[2] = comp_f_vptr; // Output array for polynomial
  comp_args[3] = comp_pder_vptr; // Partial derivatives for polynomial, NULL or undef. temp. var
  int argc = pder ? 4 : 3; // 3 or 4 args to COMP_POLY
  COMP_POLY(argc, comp_args, NULL);

  // Copy component result:
  double *comp_f = (void *) comp_f_vptr->value.arr->data;
  IDL_MEMINT Nx = x_vptr->value.arr->n_elts;
  for (int i = 0; i < Nx; i++) {
    f[i] += comp_f[i];
  }

  // Copy partial derivatives from comp_gauss to our pder
  // Our pder = array[Nx,  Na] and all pder[*,i] are consecutive,
  // i.e....
  //   comp_pder = array[Nx, cNa] and cNa = 3 (comp. has 3 parms)
  if (pder) {
    double *comp_pder = (void *) comp_pder_vptr->value.arr->data;
    for (int param = 0; param <= degree; param++) {
      for (int ix = 0; ix < Nx; ix++) {
        pder[ix + param * Nx] = comp_pder[ix + param * Nx];
      }
    }
  }

  IDL_DELTMP(comp_a_vptr);
  IDL_DELTMP(comp_f_vptr);
  if (comp_pder_vptr) {
    IDL_DELTMP(comp_pder_vptr);
  }
}

// Non-IDL-callable, used to set up calls to N * cf_gauss and cf_polyN (which is callable)
static void cf_Ng_pN(int argc, IDL_VPTR Argv[], int Ngauss, int degree)
{
  char msg[256];
  check_numeric_array_params(argc, Argv);

  IDL_VPTR x_vptr = IDL_CvtDbl(1, Argv);
  IDL_VPTR a_vptr = IDL_CvtDbl(1, Argv + 1);
  IDL_VPTR f_vptr = Argv[2]; /* Pointer to var to store output */
  IDL_VPTR pder_vptr = argc > 3 ? Argv[3] : NULL; // Partial derivatives, optional

  make_arr_0_from_template(x_vptr, f_vptr); // Also permanent, we'll return it

  double *a = (void *) a_vptr->value.arr->data;
  double *pder = NULL;

  if (pder_vptr) {
    make_pder_array(x_vptr, a_vptr, pder_vptr);
    pder = (void *) pder_vptr->value.arr->data;
  }

  IDL_MEMINT Nx = x_vptr->value.arr->n_elts;
  for (int i = 0; i < Ngauss; i++) {
    cf_gauss(x_vptr, a, f_vptr, pder);
    a += 3;
    pder = pder ? pder + 3 * Nx : NULL;
  }

  char IDL_MSG_BUFFER[256];
  int params_used = Ngauss * 3;
  cf_polyN(x_vptr, a, f_vptr, pder, degree);

  IDL_DELTMP(x_vptr);
  IDL_DELTMP(a_vptr);
}

// Non-IDL-callable, used to set up calls to N * cf_gauss and cf_polyN (which is callable)
static void cf_Ng_p0(int argc, IDL_VPTR Argv[], int Ngauss)
{
  char msg[256];
  check_numeric_array_params(argc, Argv);

  IDL_VPTR x_vptr = IDL_CvtDbl(1, Argv);
  IDL_VPTR a_vptr = IDL_CvtDbl(1, Argv + 1);
  IDL_VPTR f_vptr = Argv[2]; /* Pointer to var to store output */
  IDL_VPTR pder_vptr = argc > 3 ? Argv[3] : NULL; // Partial derivatives, optional

  make_arr_0_from_template(x_vptr, f_vptr); // Also permanent, we'll return it

  double *a = (void *) a_vptr->value.arr->data;
  double *pder = NULL;

  if (pder_vptr) {
    make_pder_array(x_vptr, a_vptr, pder_vptr);
    pder = (void *) pder_vptr->value.arr->data;
  }

  IDL_MEMINT Nx = x_vptr->value.arr->n_elts;
  for (int i = 0; i < Ngauss; i++) {
    cf_gauss(x_vptr, a, f_vptr, pder);
    a += 3;
    pder = pder ? pder + 3 * Nx : NULL;
  }

  char IDL_MSG_BUFFER[256];
  int params_used = Ngauss * 3;
  cf_polyN(x_vptr, a, f_vptr, pder, 0);

  IDL_DELTMP(x_vptr);
  IDL_DELTMP(a_vptr);
}

#define FUNC(name, Ng, degree)                                                                                         \
  static void name(int argc, IDL_VPTR Argv[], char *argk)                                                              \
  {                                                                                                                    \
    cf_Ng_pN(argc, Argv, Ng, degree);                                                                                  \
  }

FUNC(CF_G_, 1, -1);
FUNC(CF_P0_, 0, 0);
FUNC(CF_G_P0_, 1, 0);
FUNC(CF_G_G_P0_, 2, 0);
FUNC(CF_G_G_G_P0_, 3, 0);
FUNC(CF_G_G_G_G_P0_, 4, 0);
FUNC(CF_G_G_G_G_G_P0_, 5, 0);
FUNC(CF_G_G_G_G_G_G_P0_, 6, 0);
FUNC(CF_G_G_G_G_G_G_G_P0_, 7, 0);
FUNC(CF_G_G_G_G_G_G_G_G_P0_, 8, 0);
FUNC(CF_G_G_G_G_G_G_G_G_G_P0_, 9, 0);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_P0_, 10, 0);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_P0_, 11, 0);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_P0_, 12, 0);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_, 13, 0);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_, 14, 0);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_, 15, 0);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_, 16, 0);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_, 17, 0);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_, 18, 0);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_, 19, 0);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_, 20, 0);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_, 21, 0);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_, 22, 0);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_, 23, 0);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_, 24, 0);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_, 25, 0);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_, 26, 0);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_, 27, 0);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_, 28, 0);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_, 29, 0);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_, 30, 0);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_, 31, 0);
FUNC(CF_P1_, 0, 1);
FUNC(CF_G_P1_, 1, 1);
FUNC(CF_G_G_P1_, 2, 1);
FUNC(CF_G_G_G_P1_, 3, 1);
FUNC(CF_G_G_G_G_P1_, 4, 1);
FUNC(CF_G_G_G_G_G_P1_, 5, 1);
FUNC(CF_G_G_G_G_G_G_P1_, 6, 1);
FUNC(CF_G_G_G_G_G_G_G_P1_, 7, 1);
FUNC(CF_G_G_G_G_G_G_G_G_P1_, 8, 1);
FUNC(CF_G_G_G_G_G_G_G_G_G_P1_, 9, 1);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_P1_, 10, 1);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_P1_, 11, 1);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_P1_, 12, 1);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_, 13, 1);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_, 14, 1);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_, 15, 1);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_, 16, 1);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_, 17, 1);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_, 18, 1);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_, 19, 1);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_, 20, 1);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_, 21, 1);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_, 22, 1);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_, 23, 1);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_, 24, 1);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_, 25, 1);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_, 26, 1);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_, 27, 1);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_, 28, 1);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_, 29, 1);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_, 30, 1);
FUNC(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_, 31, 1);

#define ENTRY(name) {(IDL_SYSRTN_GENERIC) name, #name, 3, 4, 0, 0}

int IDL_Load(void)
{
  static IDL_SYSFUN_DEF2 pro_def[] = {
      ENTRY(COMP_POLY),
      ENTRY(COMP_GAUSS),
      ENTRY(CF_G_),
      ENTRY(CF_P0_),
      ENTRY(CF_G_P0_),
      ENTRY(CF_G_G_P0_),
      ENTRY(CF_G_G_G_P0_),
      ENTRY(CF_G_G_G_G_P0_),
      ENTRY(CF_G_G_G_G_G_P0_),
      ENTRY(CF_G_G_G_G_G_G_P0_),
      ENTRY(CF_G_G_G_G_G_G_G_P0_),
      ENTRY(CF_G_G_G_G_G_G_G_G_P0_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_P0_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_P0_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_P0_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_P0_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P0_),
      ENTRY(CF_P1_),
      ENTRY(CF_G_P1_),
      ENTRY(CF_G_G_P1_),
      ENTRY(CF_G_G_G_P1_),
      ENTRY(CF_G_G_G_G_P1_),
      ENTRY(CF_G_G_G_G_G_P1_),
      ENTRY(CF_G_G_G_G_G_G_P1_),
      ENTRY(CF_G_G_G_G_G_G_G_P1_),
      ENTRY(CF_G_G_G_G_G_G_G_G_P1_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_P1_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_P1_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_P1_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_P1_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_),
      ENTRY(CF_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_G_P1_),
  };

  return IDL_SysRtnAdd(pro_def, FALSE, sizeof(pro_def) / sizeof(IDL_SYSFUN_DEF2));
}
