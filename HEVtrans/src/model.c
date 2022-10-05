#include <R_ext/Rdynload.h>
#include "SimInf.h"

/**
 * Make sure the necessary macros are defined so that the
 * compiler can replace them when compiling the model.
 * 'SIMINF_MODEL_RUN' defines the function name of the function
 * that will be called from R to run a trajectory of the model.
 * 'SIMINF_R_INIT' is the name of the function that R will call
 * when this model is loaded into R. 'SIMINF_FORCE_SYMBOLS'
 * defines whether R allows the entry point for the run function
 * to be searched for as a character string.
 * If this file is compiled from SimInf (when calling run), the
 * macros are defined by SimInf before calling 'R CMD SHLIB'.
 * If this file is compiled as part of a package, then the
 * definitions are set in the variable 'PKG_CPPFLAGS' in
 * 'src/Makevars' and 'src/Makevars.in'.
 */
#if !defined(SIMINF_MODEL_RUN)
#  error Definition for 'SIMINF_MODEL_RUN' is missing.
#endif
#if !defined(SIMINF_R_INIT)
#  error Definition for 'SIMINF_R_INIT' is missing.
#endif
#if !defined(SIMINF_FORCE_SYMBOLS)
#  error Definition for 'SIMINF_FORCE_SYMBOLS' is missing.
#endif

/* Indices to compartments in the u state vector. */
enum{S1, E1, I1, R11, R12, R13, R14, R15, R16, M2, S2, E2, I2, R21, R22, R23, R24, R25, R26, N_COMPARTMENTS};

/* Indices to compartments in the v state vector. */
enum{PHI, CLEAN};

/* Indices in the local data vector 'ldata'. */
enum{LEFT, RIGHT, LAT, BETA, BETAEWP, BETAEBAP, CLEANRATE, GAMMA, SHEDRATE, RENEWAL, EXTBIOSEC, FARM, SLAUGHTERHOUSE};

/* Indices in the global data vector 'gdata'. */
enum{LAMBDA, DECRATE, SFRATE, QING, MU, QFECESSOW, QFECESPIG};

/* Offset -2 (each pen contains data for phi and clean) to get the
 * contamination for the left neighbor. */
const int PHI_LEFT_NEIGHBOR = -2;

/* Offset 2 (each pen contains data for phi and clean) to get the
 * contamination for the right neighbor. */
const int PHI_RIGHT_NEIGHBOR = 2;

static double
HEV_S1toE1(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    double n;

    if (ldata[SLAUGHTERHOUSE])
        return 0.0;

    n = u[S1] + u[E1] + u[I1] + u[R11] + u[R12] + u[R13] + u[R14] + u[R15] + u[R16] +
        u[M2] + u[S2] + u[E2] + u[I2] + u[R21] + u[R22] + u[R23] + u[R24] + u[R25] + u[R26];

    if (n > 0.0) {
        /* Initialise the contamination from neighbors to zero , then
         * check for neighbors. */
        double Ql = 0.0;
        double Qr = 0.0;

        /* Check if you have a neighbor at your left. */
        if (ldata[LEFT] > 0) {
            double N_left = 0.0;

            /* Since the compartments are stored as one vector in
             * memory, offset the negative number of compartments to
             * index the left pen. */
            for (int i = -N_COMPARTMENTS; i < 0; i++) {
                N_left += u[i];
            }

            if (N_left > 0.0) {
                Ql = v[PHI_LEFT_NEIGHBOR];
            }
        }

        /* Check if you have a neighbor at your right. */
        if (ldata[RIGHT] > 0) {
            double N_right = 0.0;

            for (int i = 0; i < N_COMPARTMENTS; i++) {
                N_right += u[N_COMPARTMENTS + i];
            }

            if (N_right > 0.0) {
                Qr = v[PHI_RIGHT_NEIGHBOR];
            }
        }

        return u[S1] * (ldata[BETA] * (u[I1] + u[I2]) +                 /* Direct contamination */
                        ldata[BETAEWP] * v[PHI] * gdata[QING] +         /* Within-pen environmental contamination  */
                        ldata[BETAEBAP] * gdata[QING] * (Ql + Qr)) / n; /* Between-pen environmental contamination */
    }

    return 0.0;
}

static double
HEV_S2toM2(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    if (ldata[SLAUGHTERHOUSE])
        return 0.0;

    if ((u[R11] + u[R12] + u[R13] + u[R14] + u[R15] + u[R16]) > 0 && u[S2] > 0) {
        return 1000 * u[S2];
    }

    return 0.0;
}

static double
HEV_M2toS2 (
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    if (ldata[SLAUGHTERHOUSE])
        return 0.0;

    return gdata[LAMBDA] * u[M2];
}

static double
HEV_S2toE2(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    double n;

    if (ldata[SLAUGHTERHOUSE])
        return 0.0;

    n = u[S1] + u[E1] + u[I1] + u[R11] + u[R12] + u[R13] + u[R14] + u[R15] + u[R16] +
        u[M2] + u[S2] + u[E2] + u[I2] + u[R21] + u[R22] + u[R23] + u[R24] + u[R25] + u[R26];

    if (n > 0.0) {
        /* Initialise the contamination from neighbors to zero , then
         * check for neighbors. */
        double Ql = 0.0;
        double Qr = 0.0;

        /* Check if you have a neighbor at your left. */
        if (ldata[LEFT] > 0) {
            double N_left = 0.0;

            /* Since the compartments are stored as one vector in
             * memory, offset the negative number of compartments to
             * index the left pen. */
            for (int i = -N_COMPARTMENTS; i < 0; i++) {
                N_left += u[i];
            }

            if (N_left > 0.0) {
                Ql = v[PHI_LEFT_NEIGHBOR];
            }
        }

        /* Check if you have a neighbor at your right. */
        if (ldata[RIGHT] > 0) {
            double N_right = 0.0;

            for (int i = 0; i < N_COMPARTMENTS; i++) {
                N_right += u[N_COMPARTMENTS + i];
            }

            if (N_right > 0.0) {
                Qr = v[PHI_RIGHT_NEIGHBOR];
            }
        }

        return u[S2] * (ldata[BETA] * (u[I1] + u[I2]) +                 /* Direct contamination */
                        ldata[BETAEWP] * v[PHI] * gdata[QING] +         /* Within-pen environmental contamination */
                        ldata[BETAEBAP] * gdata[QING] * (Ql + Qr)) / n; /* Between-pen environmental contamination */
    }

    return 0.0;
}

static double
HEV_E1toI1(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    if (ldata[SLAUGHTERHOUSE])
        return 0.0;

    return ldata[LAT] * u[E1];
}

static double
HEV_E2toI2(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    if (ldata[SLAUGHTERHOUSE])
        return 0.0;

    return ldata[LAT] * u[E2];
}

static double
HEV_I1toR11(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    if (ldata[SLAUGHTERHOUSE])
        return 0.0;

    return ldata[GAMMA] * u[I1];
}

static double
HEV_I2toR21(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    if (ldata[SLAUGHTERHOUSE])
        return 0.0;

    return ldata[GAMMA] * u[I2];
}

static double
HEV_R11toR12(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    if (ldata[SLAUGHTERHOUSE])
        return 0.0;

    return gdata[MU] * u[R11];
}

static double
HEV_R12toR13(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    if (ldata[SLAUGHTERHOUSE])
        return 0.0;

    return gdata[MU] * u[R12];
}

static double
HEV_R13toR14(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    if (ldata[SLAUGHTERHOUSE])
        return 0.0;

    return gdata[MU] * u[R13];
}

static double
HEV_R14toR15(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    if (ldata[SLAUGHTERHOUSE])
        return 0.0;

    return gdata[MU] * u[R14];
}

static double
HEV_R15toR16(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    if (ldata[SLAUGHTERHOUSE])
        return 0.0;

    return gdata[MU] * u[R15];
}

static double
HEV_R16toS1(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    if (ldata[SLAUGHTERHOUSE])
        return 0.0;

    return gdata[MU] * u[R16];
}

static double
HEV_R21toR22(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    if (ldata[SLAUGHTERHOUSE])
        return 0.0;

    return gdata[MU] * u[R21];
}

static double
HEV_R22toR23(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    if (ldata[SLAUGHTERHOUSE])
        return 0.0;

    return gdata[MU] * u[R22];
}

static double
HEV_R23toR24(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    if (ldata[SLAUGHTERHOUSE])
        return 0.0;

    return gdata[MU] * u[R23];
}

static double
HEV_R24toR25(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    if (ldata[SLAUGHTERHOUSE])
        return 0.0;

    return gdata[MU] * u[R24];
}

static double
HEV_R25toR26(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    if (ldata[SLAUGHTERHOUSE])
        return 0.0;

    return gdata[MU] * u[R25];
}

static double
HEV_R26toS2(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    if (ldata[SLAUGHTERHOUSE])
        return 0.0;

    return gdata[MU] * u[R26];
}

/**
 * Post time step function.
 *
 * @param v_new If a continuous state vector is used by a model,
 *        this is the new continuous state vector in the node after
 *        the post time step.
 * @param u The compartment state vector in the node.
 * @param v The current continuous state vector in the node.
 * @param ldata The local data vector in the node.
 * @param gdata The global data vector that is common to all nodes.
 * @param node The node index. Note the node index is zero-based,
 *        i.e., the first node is 0.
 * @param t Current time in the simulation.
 * @return error code (<0), or 1 if node needs to update the
 *         transition rates, or 0 when it doesn't need to update
 *         the transition rates.
 */
static int
ptsFun(
    double *v_new,
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    int node,
    double t)
{
    double N_i = 0.0;

    if (ldata[SLAUGHTERHOUSE])
        return 0.0;

    /* Count the number of animals in the pen. */
    for (int i = 0; i < N_COMPARTMENTS; i++) {
        N_i += u[i];
    }

    /* Decay of the environmental infectious pressure. Forward Euler
     * step. */
    if (N_i > 0.0) {
        v_new[PHI] = v[PHI] * (1.0 - gdata[DECRATE]) * (1.0 - gdata[SFRATE]) + ldata[EXTBIOSEC];
        v_new[CLEAN] = 0.0;
    } else if ((int)v[CLEAN] == 0) {
        v_new[PHI] = v[PHI] * (1.0 - ldata[CLEANRATE]);
        v_new[CLEAN] = 1.0;
    }

    if (N_i > 0.0) {
        /* Contamination */
        v_new[PHI] += ldata[SHEDRATE] * (gdata[QFECESSOW]*u[I1] + gdata[QFECESPIG] * u[I2])
            / ((u[S1] + u[E1] + u[I1] + u[R11] + u[R12] + u[R13] + u[R14] + u[R15] + u[R16]) * gdata[QFECESSOW] +
               (u[M2] + u[S2] + u[E2] + u[I2] + u[R21] + u[R22] + u[R23] + u[R24] + u[R25] + u[R26]) * gdata[QFECESPIG]);
    }

    if (!isfinite(v_new[PHI]))
        return -1;
    if (v_new[PHI] < 0.0)
        return -1;
    return v[PHI] != v_new[PHI]; /* 1 if needs update */
}

/**
 * Run a trajectory of the model.
 *
 * @param model The model.
 * @param solver The name of the numerical solver.
 * @return A model with a trajectory attached to it.
 */
static SEXP SIMINF_MODEL_RUN(SEXP model, SEXP solver)
{
    static SEXP(*SimInf_run)(SEXP, SEXP, TRFun*, PTSFun) = NULL;
    TRFun tr_fun[] = {&HEV_S1toE1,&HEV_E1toI1,&HEV_I1toR11,
        &HEV_R11toR12,&HEV_R12toR13,&HEV_R13toR14,
        &HEV_R14toR15,&HEV_R15toR16,&HEV_R16toS1,
        &HEV_S2toM2, &HEV_M2toS2,&HEV_S2toE2,
        &HEV_E2toI2,&HEV_I2toR21,&HEV_R21toR22,
        &HEV_R22toR23,&HEV_R23toR24,&HEV_R24toR25,
        &HEV_R25toR26,&HEV_R26toS2};

    if (!SimInf_run) {
        SimInf_run = (SEXP(*)(SEXP, SEXP, TRFun*, PTSFun))
            R_GetCCallable("SimInf", "SimInf_run");

        if (!SimInf_run) {
            Rf_error("Cannot find function 'SimInf_run'.");
        }
    }

    return SimInf_run(model, solver, tr_fun, &ptsFun);
}

/**
 * A NULL-terminated array of routines to register for the .Call
 * interface, see section '5.4 Registering native routines' in
 * the 'Writing R Extensions' manual.
 */
static const R_CallMethodDef callMethods[] =
{
    SIMINF_CALLDEF(SIMINF_MODEL_RUN, 2),
    {NULL, NULL, 0}
};

/**
 * This routine will be invoked when R loads the shared object/DLL,
 * see section '5.4 Registering native routines' in the
 * 'Writing R Extensions' manual.
 */
void SIMINF_R_INIT(DllInfo *info)
{
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
    R_forceSymbols(info, SIMINF_FORCE_SYMBOLS);
}
