#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <idl_export.h>

#define MASK 1 /* Enable/disable keyword mask */
#define IKWOF(a) IDL_KW_OFFSETOF(a)

static void bailout(char *msg)
{
  IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_LONGJMP, msg);
}

static void info(char *msg)
{
  IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, msg);
}

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))

/* Quicksort-based (partitioning) k'th element finder
   Based on http://www.mathcs.carleton.edu/courses/course_resources/
   cs227_w96/swanjorg/algorithms.html

   INPUTS:
   A - Array with data (N elements)
   K - The number of the element to be found
   N - The number of elements in A
*/

#define SWAP(a, b) (atemp = a, a = b, b = atemp)

#define QSRT_k_func(PFX, type)                       \
  static void PFX##_qsrt_k(type a[], long k, long n) \
  {                                                  \
    type av, atemp;                                  \
    long i, j, l, r, test;                           \
    l = 0;                                           \
    r = n - 1;                                       \
                                                     \
    while (r > l)                                    \
    {                                                \
      av = a[r];                                     \
      i = l - 1;                                     \
      j = r;                                         \
                                                     \
      test = 1;                                      \
      while (test)                                   \
      {                                              \
        ++i;                                         \
        while ((a[i] < av) && (i < j))               \
          ++i;                                       \
        --j;                                         \
        while ((a[j] > av) && (j > i))               \
          --j;                                       \
        if (i < j)                                   \
          SWAP(a[i], a[j]);                          \
        else                                         \
          test = 0;                                  \
      }                                              \
                                                     \
      SWAP(a[i], a[r]);                              \
                                                     \
      /* Terminate if appropriate */                 \
      if (i == k)                                    \
        return;                                      \
                                                     \
      /* Select the correct interval */              \
      if (k < i)                                     \
        r = i - 1;                                   \
      else                                           \
        l = i + 1;                                   \
    }                                                \
    return;                                          \
  }

QSRT_k_func(c, UCHAR);
QSRT_k_func(i, IDL_INT);
QSRT_k_func(ui, IDL_UINT);
QSRT_k_func(l, IDL_LONG);
QSRT_k_func(ul, IDL_ULONG);
QSRT_k_func(l64, IDL_LONG64);
QSRT_k_func(ul64, IDL_ULONG64);
QSRT_k_func(f, float);
QSRT_k_func(d, double);

#define FMEDIAN_CORE(TYPE, V, ALWAYS)                                      \
  {                                                                        \
    TYPE *in = (TYPE *)a->value.arr->data;                                 \
    TYPE *out = (TYPE *)res->value.arr->data;                              \
    TYPE *workspace = (TYPE *)Workspace->value.arr->data;                  \
                                                                           \
    /* If widths given for median filter are out of range, */              \
    /* return original array unmodified.  */                               \
                                                                           \
    if ((((n_width1 > n_width2) ? n_width1 : n_width2) < 2) ^ (ndim1 < 1)) \
    {                                                                      \
      for (i = 0; i < (ndim1 * ndim2); ++i)                                \
        out[i] = in[i];                                                    \
    }                                                                      \
    else                                                                   \
    {                                                                      \
                                                                           \
      if (!kw.MISSING)                                                     \
      {                                                                    \
        for (k = 0; k < a->value.arr->n_elts; k++)                         \
          Miss->value.V = MIN(*(in + k), Miss->value.V);                   \
        Miss->value.V -= 1;                                                \
      }                                                                    \
                                                                           \
      /* Calculate the half width points. */                               \
                                                                           \
      nw1 = (n_width1 - 1) / 2;                                            \
      nw2 = (n_width2 - 1) / 2;                                            \
                                                                           \
      /* Find box with points to use */                                    \
      /* Careful not to exceed the limits of the input array */            \
                                                                           \
      for (j = 0; j < ndim2; ++j)                                          \
      {                                                                    \
        j1 = MAX(j - nw2, 0);                                              \
        j2 = MIN(j + n_width2 - nw2 - 1, ndim2 - 1);                       \
        for (i = 0; i < ndim1; ++i)                                        \
        {                                                                  \
          i1 = MAX(i - nw1, 0);                                            \
          i2 = MIN(i + n_width1 - nw1 - 1, ndim1 - 1);                     \
                                                                           \
          out[i + ndim1 * j] = in[i + ndim1 * j]; /* Default value */      \
                                                                           \
          /* Store & count points from box into WORKSPACE */               \
          if (ALWAYS || in[i + ndim1 * j] == Miss->value.V)                \
          {                                                                \
            for (k = 0, jj = j1; jj <= j2; ++jj)                           \
              for (ii = i1; ii <= i2; ++ii)                                \
                if (in[ii + ndim1 * jj] != Miss->value.V)                  \
                  workspace[k++] = in[ii + ndim1 * jj];                    \
                                                                           \
            /* If no points, leave default value [MISSING] */              \
            /* If only one or two points, do the average. */               \
            /* Otherwise, sort in ascending order, and find midpoint. */   \
                                                                           \
            if (k == 1 || k == 2)                                          \
            {                                                              \
              out[i + ndim1 * j] = 0;                                      \
              long n_sort = k;                                             \
              for (k = 0; k < n_sort; ++k)                                 \
                out[i + ndim1 * j] += workspace[k];                        \
              out[i + ndim1 * j] = out[i + ndim1 * j] / n_sort;            \
            }                                                              \
            else if (k)                                                    \
            {                                                              \
              V##_qsrt_k(workspace, k / 2, k);                             \
              out[i + ndim1 * j] = workspace[k / 2];                       \
            }                                                              \
          }                                                                \
        }                                                                  \
      }                                                                    \
    }                                                                      \
  }

IDL_VPTR FMEDIAN(int argc, IDL_VPTR argv[], char *argk)
{
  typedef struct
  {
    IDL_KW_RESULT_FIRST_FIELD;
    IDL_VPTR MISSING;
    IDL_LONG ONLY_MISSING;
  } KW_RESULT;

  KW_RESULT kw;

  static IDL_KW_PAR kw_pars[] = {/* Note: must be in lexical order! */
                                 /* name     type            flags  &specified    &value     */
                                 {"MISSING", 0, MASK, IDL_KW_VIN | IDL_KW_ZERO, 0, IKWOF(MISSING)},
                                 {"ONLY_MISSING", IDL_TYP_LONG, MASK, IDL_KW_ZERO | IDL_KW_VALUE | 1, 0, IKWOF(ONLY_MISSING)},
                                 {NULL}};
  IDL_VPTR Argv[3]; /* plain-args, maximum 2 */
  argc = IDL_KWProcessByOffset(argc, argv, argk, kw_pars, Argv, MASK, &kw);

  IDL_VPTR a = Argv[0]; /* Input array */
  IDL_VPTR b = Argv[1]; /* Box size 1 */
  IDL_VPTR c = Argv[2]; /* Box size 2 */
  IDL_VPTR Workspace;
  IDL_VPTR call[1]; /* For use when calling conversion routines */
  IDL_VPTR res;     /* Temp. var to be returned as the result */
  IDL_VPTR Miss;

  /* TYPE CHECKING / ALLOCATION SECTION */

  IDL_EXCLUDE_STRING(a);
  IDL_EXCLUDE_COMPLEX(a);
  IDL_ENSURE_SIMPLE(a); /* Excl: FILE, STC, PTR, OBJ */
  IDL_ENSURE_ARRAY(a);

  int always = !kw.ONLY_MISSING;

  if (a->value.arr->n_dim != 2)
    bailout("Only 2-dimensional arrays allowed");

  if (kw.MISSING)
  {
    IDL_ENSURE_SCALAR(kw.MISSING);
    call[0] = kw.MISSING;
    Miss = IDL_BasicTypeConversion(1, call, a->type);
  }
  else
  {
    Miss = IDL_Gettmp();
  }

  IDL_VarMakeTempFromTemplate(a, a->type, NULL /*structdef*/, &res, FALSE);

  /* Ensure box dimensions are longs */
  call[0] = b;
  b = IDL_CvtLng(1, call);
  call[0] = c;
  c = IDL_CvtLng(1, call);

  /* Make workspace: */
  IDL_MakeTempVector(a->type, b->value.l * c->value.l, IDL_ARR_INI_NOP, &Workspace);

  /* These variables are used by FMEDIAN_CORE, but is unchanged
     for all different input data types */
  IDL_LONG ndim1 = a->value.arr->dim[0];
  IDL_LONG ndim2 = a->value.arr->dim[1];
  IDL_LONG n_width1 = b->value.l;
  IDL_LONG n_width2 = c->value.l;
  long i, j, k, ii, jj, i1, i2, j1, j2, nw1, nw2;

  switch (a->type)
  {
  case 1:
    FMEDIAN_CORE(UCHAR, c, always);
    break;
  case 2:
    FMEDIAN_CORE(IDL_INT, i, always);
    break;
  case 3:
    FMEDIAN_CORE(IDL_LONG, l, always);
    break;
  case 4:
    FMEDIAN_CORE(float, f, always);
    break;
  case 5:
    FMEDIAN_CORE(double, d, always);
    break;
  case IDL_TYP_UINT:
    FMEDIAN_CORE(IDL_UINT, ui, always);
    break;
  case IDL_TYP_ULONG:
    FMEDIAN_CORE(IDL_ULONG, ul, always);
    break;
  case IDL_TYP_LONG64:
    FMEDIAN_CORE(IDL_LONG64, l64, always);
    break;
  case IDL_TYP_ULONG64:
    FMEDIAN_CORE(IDL_ULONG64, ul64, always);
    break;
  default:
    bailout("An unsupported type was supplied!");
    break;
  }

  if (a != Argv[0])
    IDL_DELTMP(a);
  if (b != Argv[1])
    IDL_DELTMP(b);
  if (c != Argv[2])
    IDL_DELTMP(c);
  IDL_DELTMP(Workspace);

  if (Miss != kw.MISSING)
    IDL_DELTMP(Miss);
  IDL_KW_FREE;

  return res;
}

int IDL_Load(void)
{
  static IDL_SYSFUN_DEF2 func_def[] =
      {{{FMEDIAN}, "FMEDIAN", 3, 3, IDL_SYSFUN_DEF_F_KEYWORDS, 0}};

  return IDL_SysRtnAdd(func_def, TRUE, 1);
}
