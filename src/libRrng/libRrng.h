#ifndef __LIB_R_RNG_H
#define __LIB_R_RNG_H

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>
#include <limits.h>
#include <float.h>

#define MATHLIB_STANDALONE

#ifdef  __cplusplus
extern "C" {
#endif

//#include <R_ext/Boolean.h>

typedef enum {
    WICHMANN_HILL,
    MARSAGLIA_MULTICARRY,
    SUPER_DUPER,
    MERSENNE_TWISTER,
    KNUTH_TAOCP,
    USER_UNIF,
    KNUTH_TAOCP2,
    LECUYER_CMRG
} RNGtype;

/* Different kinds of "N(0,1)" generators :*/
typedef enum {
    BUGGY_KINDERMAN_RAMAGE,
    AHRENS_DIETER,
    BOX_MULLER,
    USER_NORM,
    INVERSION,
    KINDERMAN_RAMAGE
} N01type;


void GetRNGstate();
void PutRNGstate();

void set_Random_seed(const int* data, int n);
void get_Random_seed(int* data, int n);


double unif_rand(void);
/* These are also defined in Rmath.h */
double norm_rand(void);
double exp_rand(void);

typedef unsigned int Int32;
//double * user_unif_rand(void);
//void user_unif_init(Int32);
//int * user_unif_nseed(void);
//int * user_unif_seedloc(void);
//
//double * user_norm_rand(void);
//
//void FixupProb(double *, int, int, Rboolean);

// Export other functions for setting/getting the RNG state
void do_setseed (Int32 seed, RNGtype* skind, N01type* nkind);

/**
 * Extract the RNG seed as .Random.seed: the first element encodes the RNG and
 * Normal kind.
 *
 * @param rseed output seed (must be at least 626 long)
 */
int do_getseed(int* seed);

// Export other stat functions
double rgamma(double a, double scale);

double qnorm5(double p, double mu, double sigma, int lower_tail, int log_p);
#define qnorm qnorm5;

#ifdef  __cplusplus
}
#endif

#define MAX_SEED_LENGTH 625

// extra defines
#define ISNAN(x)     (isnan(x)!=0)
#define ML_POSINF	1.0/0.0
#define ML_NEGINF	-1.0/0.0
#define ML_NAN 0.0/0.0

#define ML_ERR_return_NAN { return ML_NAN; }

/* Redefinitions from errors.c */
#define warning printf
#define error printf

/* From Defn.h */
#define _(String) (String)

#endif
