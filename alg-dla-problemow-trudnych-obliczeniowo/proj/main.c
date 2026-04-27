/*
 * ==========================================================================
 *  Kotron Puzzle Solver — SAT Reduction + Embedded MicroSAT CDCL
 * ==========================================================================
 *
 *  Problem:
 *    Given an H×W grid containing lasers (A/V/</>), cats (O), walls (#),
 *    and empty cells (.), place at most L mirrors (/ or \) on empty cells
 *    so that every cat is illuminated by at least one laser beam.
 *    Light travels in a straight line. Only walls block light; lasers and
 *    cats are transparent. Mirrors reflect light at 90 degrees:
 *      "/" reflects:  E→N, N→E, W→S, S→W
 *      "\" reflects:  E→S, S→E, W→N, N→W
 *
 *  Approach:
 *    Reduce Kotron to SAT and solve with an embedded MicroSAT CDCL solver.
 *    After finding a satisfying assignment, simulate actual light paths
 *    from lasers and verify all cats are lit. If not (phantom cycle),
 *    add a no-good clause blocking the phantom mirrors and re-solve.
 *
 *  Boolean Variables (all 1-indexed):
 *
 *    M(i,j)   = 1 + i*W + j
 *      True iff cell (i,j) contains a mirror.
 *
 *    T(i,j)   = 1 + H*W + i*W + j
 *      Mirror type: true = "\" (backslash), false = "/" (slash).
 *      Only meaningful when M(i,j) is true.
 *
 *    R(i,j,d) = 1 + 2*H*W + (i*W + j)*4 + d     where d in {DN,DS,DE,DW}
 *      True iff a laser beam passes through cell (i,j) traveling in
 *      direction d.
 *
 *    SC(k,j)  = sc_base + (k-1)*L + (j-1)         sc_base = 1 + 6*H*W
 *      Sinz sequential counter auxiliary variables for the at-most-K
 *      cardinality constraint on mirror count.
 *
 *  Clause Groups:
 *
 *    1. Fixed cells:
 *       - Walls: R(i,j,d) = false for all d
 *       - Lasers: M(i,j) = false
 *       - Cats: M(i,j) = false
 *
 *    2. Laser sources:
 *       - Each laser forces R = true on the adjacent cell in its
 *         emission direction.  e.g. '>' at (i,j) → R(i,j+1,DE) = true.
 *
 *    3. Forward propagation (for non-wall cells):
 *       - No mirror: R(i,j,d) ∧ ¬M(i,j) → R(neighbor,d)
 *       - Slash mirror "/": R(i,j,d_in) ∧ ¬M(i,j) ∧ T=0 → R(neighbor,d_out)
 *       - Backslash mirror "\": R(i,j,d_in) ∧ ¬M(i,j) ∧ T=1 → R(neighbor,d_out)
 *       These are encoded as 3-literal and 4-literal clauses.
 *
 *    4. Reverse propagation (for non-wall cells):
 *       If R(i,j,d) is true, the beam must have a valid source in the
 *       predecessor cell (the cell from which direction d originates).
 *       Three cases per (cell, direction):
 *       - Predecessor has no mirror → R must propagate straight
 *       - Predecessor has "/" → R must come from the slash-reflected dir
 *       - Predecessor has "\" → R must come from the backslash-reflected dir
 *       If predecessor is a wall/outside, force R(i,j,d) = false.
 *
 *    5. Cat illumination:
 *       For each cat at (i,j): R(i,j,DN) ∨ R(i,j,DS) ∨ R(i,j,DE) ∨ R(i,j,DW)
 *
 *    6. At-most-L mirrors (Sinz sequential counter):
 *       Encodes sum(M(i,j) for empty cells) ≤ L using O(n*L) auxiliary vars.
 *
 *  Phantom Cycle Problem:
 *    The SAT encoding's reverse propagation prevents single phantom rays
 *    but NOT cycles: 4+ mirrors can form a closed light loop where each
 *    mirror has a valid predecessor (the previous mirror in the loop).
 *    These satisfy all clauses but don't correspond to real laser light.
 *
 *    Fix: after solve(), simulate real light from all lasers. If any cat
 *    is unlit, identify phantom mirrors (M=1 but not on any real light
 *    path) and add a no-good clause blocking that phantom placement.
 *    Re-initialize the solver and re-solve. Repeat up to 100 times.
 *
 *  Complexity:
 *    Variables: O(H*W + n_empty*L)   Clauses: O(H*W + n_empty*L)
 *    For checkpoint A (≤10×10, ≤4 mirrors): very fast.
 *    Memory: dynamically sized, roughly 10*nVars + 7*nClauses + 500K ints.
 *
 * ==========================================================================
 */

#include <stdio.h>
#include <stdlib.h>

/* =========================================================================
   Part 1: MicroSAT CDCL core (Marijn Heule, MIT License)
   Modified: added phase saving in unassign(), removed parse()/main(),
   renamed 'false' array to 'falseLit', dynamic mem_max.
   ========================================================================= */

enum { END = -9, UNSAT = 0, SAT = 1, MARK = 2, IMPLIED = 6 };

struct solver {
  int *DB, nVars, nClauses, mem_used, mem_fixed, mem_max, maxLemmas, nLemmas,
      *buffer, nConflicts, *model, *reason, *falseStack, *falseLit, *first,
      *forced, *processed, *assigned, *next, *prev, head, res, fast, slow;
};

static void unassign(struct solver *S, int lit) {
  S->model[abs(lit)] = (lit > 0); /* phase saving: remember last polarity */
  S->falseLit[lit] = 0;
}

static void restart(struct solver *S) {
  while (S->assigned > S->forced)
    unassign(S, *(--S->assigned));
  S->processed = S->forced;
}

static void assign(struct solver *S, int *reason, int forced) {
  int lit = reason[0];
  S->falseLit[-lit] = forced ? IMPLIED : 1;
  *(S->assigned++) = -lit;
  S->reason[abs(lit)] = 1 + (int)(reason - S->DB);
  S->model[abs(lit)] = (lit > 0);
}

static void addWatch(struct solver *S, int lit, int mem) {
  S->DB[mem] = S->first[lit];
  S->first[lit] = mem;
}

static int *getMemory(struct solver *S, int mem_size) {
  if (S->mem_used + mem_size > S->mem_max) {
    printf("c out of memory\n");
    exit(1);
  }
  int *store = S->DB + S->mem_used;
  S->mem_used += mem_size;
  return store;
}

static int *sat_addClause(struct solver *S, int *in, int size, int irr) {
  int i, used = S->mem_used;
  int *clause = getMemory(S, size + 3) + 2;
  if (size > 1) {
    addWatch(S, in[0], used);
    addWatch(S, in[1], used + 1);
  }
  for (i = 0; i < size; i++)
    clause[i] = in[i];
  clause[i] = 0;
  if (irr)
    S->mem_fixed = S->mem_used;
  else
    S->nLemmas++;
  return clause;
}

static void reduceDB(struct solver *S, int k) {
  while (S->nLemmas > S->maxLemmas)
    S->maxLemmas += 300;
  S->nLemmas = 0;
  int i;
  for (i = -S->nVars; i <= S->nVars; i++) {
    if (i == 0)
      continue;
    int *watch = &S->first[i];
    while (*watch != END)
      if (*watch < S->mem_fixed)
        watch = S->DB + *watch;
      else
        *watch = S->DB[*watch];
  }
  int old_used = S->mem_used;
  S->mem_used = S->mem_fixed;
  for (i = S->mem_fixed + 2; i < old_used; i += 3) {
    int count = 0, head_pos = i;
    while (S->DB[i]) {
      int lit = S->DB[i++];
      if ((lit > 0) == S->model[abs(lit)])
        count++;
    }
    if (count < k)
      sat_addClause(S, S->DB + head_pos, i - head_pos, 0);
  }
}

static void bump(struct solver *S, int lit) {
  if (S->falseLit[lit] != IMPLIED) {
    S->falseLit[lit] = MARK;
    int var = abs(lit);
    if (var != S->head) {
      S->prev[S->next[var]] = S->prev[var];
      S->next[S->prev[var]] = S->next[var];
      S->next[S->head] = var;
      S->prev[var] = S->head;
      S->head = var;
    }
  }
}

static int implied(struct solver *S, int lit) {
  if (S->falseLit[lit] > MARK)
    return (S->falseLit[lit] & MARK);
  if (!S->reason[abs(lit)])
    return 0;
  int *p = S->DB + S->reason[abs(lit)] - 1;
  while (*(++p))
    if ((S->falseLit[*p] ^ MARK) && !implied(S, *p)) {
      S->falseLit[lit] = IMPLIED - 1;
      return 0;
    }
  S->falseLit[lit] = IMPLIED;
  return 1;
}

static int *analyze(struct solver *S, int *clause) {
  S->res++;
  S->nConflicts++;
  while (*clause)
    bump(S, *(clause++));
  while (S->reason[abs(*(--S->assigned))]) {
    if (S->falseLit[*S->assigned] == MARK) {
      int *check = S->assigned;
      while (S->falseLit[*(--check)] != MARK)
        if (!S->reason[abs(*check)])
          goto build;
      clause = S->DB + S->reason[abs(*S->assigned)];
      while (*clause)
        bump(S, *(clause++));
    }
    unassign(S, *S->assigned);
  }
build:;
  int size = 0, lbd = 0, flag = 0;
  int *p = S->processed = S->assigned;
  while (p >= S->forced) {
    if ((S->falseLit[*p] == MARK) && !implied(S, *p)) {
      S->buffer[size++] = *p;
      flag = 1;
    }
    if (!S->reason[abs(*p)]) {
      lbd += flag;
      flag = 0;
      if (size == 1)
        S->processed = p;
    }
    S->falseLit[*(p--)] = 1;
  }
  S->fast -= S->fast >> 5;
  S->fast += lbd << 15;
  S->slow -= S->slow >> 15;
  S->slow += lbd << 5;
  while (S->assigned > S->processed)
    unassign(S, *(S->assigned--));
  unassign(S, *S->assigned);
  S->buffer[size] = 0;
  return sat_addClause(S, S->buffer, size, 0);
}

static int propagate(struct solver *S) {
  int forced = S->reason[abs(*S->processed)];
  while (S->processed < S->assigned) {
    int lit = *(S->processed++);
    int *watch = &S->first[lit];
    while (*watch != END) {
      int i, unit = 1;
      int *clause = S->DB + *watch + 1;
      if (clause[-2] == 0)
        clause++;
      if (clause[0] == lit)
        clause[0] = clause[1];
      for (i = 2; unit && clause[i]; i++)
        if (!S->falseLit[clause[i]]) {
          clause[1] = clause[i];
          clause[i] = lit;
          int store = *watch;
          unit = 0;
          *watch = S->DB[*watch];
          addWatch(S, clause[1], store);
        }
      if (unit) {
        clause[1] = lit;
        watch = S->DB + *watch;
        if (S->falseLit[-clause[0]])
          continue;
        if (!S->falseLit[clause[0]])
          assign(S, clause, forced);
        else {
          if (forced)
            return UNSAT;
          int *lemma = analyze(S, clause);
          if (!lemma[1])
            forced = 1;
          assign(S, lemma, forced);
          break;
        }
      }
    }
  }
  if (forced)
    S->forced = S->processed;
  return SAT;
}

static int solve(struct solver *S) {
  int decision = S->head;
  S->res = 0;
  for (;;) {
    int old_nLemmas = S->nLemmas;
    if (propagate(S) == UNSAT)
      return UNSAT;
    if (S->nLemmas > old_nLemmas) {
      decision = S->head;
      if (S->fast > (S->slow / 100) * 125) {
        S->res = 0;
        S->fast = (S->slow / 100) * 125;
        restart(S);
        if (S->nLemmas > S->maxLemmas)
          reduceDB(S, 6);
      }
    }
    while (S->falseLit[decision] || S->falseLit[-decision])
      decision = S->prev[decision];
    if (decision == 0)
      return SAT;
    decision = S->model[decision] ? decision : -decision;
    S->falseLit[-decision] = 1;
    *(S->assigned++) = -decision;
    decision = abs(decision);
    S->reason[decision] = 0;
  }
}

static void initCDCL(struct solver *S, int n, int m) {
  if (n < 1)
    n = 1;
  S->nVars = n;
  S->nClauses = m;
  /* Dynamic sizing: arrays(~9n) + clauses(m*7) + lemmas + margin */
  S->mem_max = 10 * (n + 1) + m * 7 + 500000;
  S->mem_used = 0;
  S->nLemmas = 0;
  S->nConflicts = 0;
  S->maxLemmas = 2000;
  S->fast = S->slow = 1 << 24;

  S->DB = (int *)malloc(sizeof(int) * S->mem_max);
  S->model = getMemory(S, n + 1);
  S->next = getMemory(S, n + 1);
  S->prev = getMemory(S, n + 1);
  S->buffer = getMemory(S, n);
  S->reason = getMemory(S, n + 1);
  S->falseStack = getMemory(S, n + 1);
  S->forced = S->falseStack;
  S->processed = S->falseStack;
  S->assigned = S->falseStack;
  S->falseLit = getMemory(S, 2 * n + 1);
  S->falseLit += n;
  S->first = getMemory(S, 2 * n + 1);
  S->first += n;
  S->DB[S->mem_used++] = 0;

  int i;
  for (i = 1; i <= n; i++) {
    S->prev[i] = i - 1;
    S->next[i - 1] = i;
    S->model[i] = S->falseLit[-i] = S->falseLit[i] = 0;
    S->first[i] = S->first[-i] = END;
  }
  S->head = n;
}

/* =========================================================================
   Part 2: Variable numbering + clause-adding wrapper
   ========================================================================= */

#define MAX_GRID 101
#define DN 0
#define DS 1
#define DE 2
#define DW 3

static int gW, gH, gL;
static char grid[MAX_GRID][MAX_GRID];

/* Direct variable numbering (1-indexed, no hash map) */
static inline int var_M(int i, int j) { return 1 + i * gW + j; }
static inline int var_T(int i, int j) { return 1 + gH * gW + i * gW + j; }
static inline int var_R(int i, int j, int d) {
  return 1 + 2 * gH * gW + (i * gW + j) * 4 + d;
}

static int sc_base; /* set after reading input */
static inline int var_SC(int k, int j) {
  return sc_base + (k - 1) * gL + (j - 1);
}

/* Wrapper: add clause with unit propagation handling */
static int add_clause_buf[8];
static int solver_ok; /* 0 = still ok, 1 = UNSAT during add */

static void add_clause(struct solver *S, int *lits, int size) {
  if (!solver_ok)
    return;
  int *clause = sat_addClause(S, lits, size, 1);
  if (!size || (size == 1 && S->falseLit[clause[0]])) {
    solver_ok = 0;
    return;
  }
  if (size == 1 && !S->falseLit[-clause[0]])
    assign(S, clause, 1);
}

/* Convenience helpers for creating clauses */
static void clause1(struct solver *S, int a) {
  add_clause_buf[0] = a;
  add_clause(S, add_clause_buf, 1);
}
static void clause2(struct solver *S, int a, int b) {
  add_clause_buf[0] = a;
  add_clause_buf[1] = b;
  add_clause(S, add_clause_buf, 2);
}
static void clause3(struct solver *S, int a, int b, int c) {
  add_clause_buf[0] = a;
  add_clause_buf[1] = b;
  add_clause_buf[2] = c;
  add_clause(S, add_clause_buf, 3);
}
static void clause4(struct solver *S, int a, int b, int c, int d) {
  add_clause_buf[0] = a;
  add_clause_buf[1] = b;
  add_clause_buf[2] = c;
  add_clause_buf[3] = d;
  add_clause(S, add_clause_buf, 4);
}

/* =========================================================================
   Part 3: Kotron → SAT reduction
   ========================================================================= */

static int is_blocking(int i, int j) {
  return grid[i][j] == '#';
}

static int inside(int i, int j) { return i >= 0 && i < gH && j >= 0 && j < gW; }

/* Direction tables */
static const int dx[4] = {-1, 1, 0, 0}; /* DN, DS, DE, DW */
static const int dy[4] = {0, 0, 1, -1};

/* Reverse: source offset for a beam entering in direction d */
static const int src_dx[4] = {1, -1, 0, 0};
static const int src_dy[4] = {0, 0, -1, 1};

/* Mirror reflection: incoming direction d → which direction was the source?  */
/* "/" (T=0): E→N, N→E, W→S, S→W */
static const int slash_in_d[4] = {DE, DW, DN, DS}; /* for d=DN,DS,DE,DW */
/* "\" (T=1): E→S, S→E, W→N, N→W */
static const int back_in_d[4] = {DW, DE, DS, DN};

/* Which laser character emits in direction d */
static char laser_for_dir(int d) {
  if (d == DN)
    return 'A';
  if (d == DS)
    return 'V';
  if (d == DE)
    return '>';
  return '<';
}

/* Forward reflection tables: (d_in, di, dj, d_out) */
/* "/" mirror: */
static const int slash_fwd[4][4] = {
    /* DE→DN */ {DE, -1, 0, DN},
    /* DN→DE */ {DN, 0, 1, DE},
    /* DW→DS */ {DW, 1, 0, DS},
    /* DS→DW */ {DS, 0, -1, DW}};
/* "\" mirror: */
static const int back_fwd[4][4] = {
    /* DE→DS */ {DE, 1, 0, DS},
    /* DS→DE */ {DS, 0, 1, DE},
    /* DW→DN */ {DW, -1, 0, DN},
    /* DN→DW */ {DN, 0, -1, DW}};

static int mirror_cells[MAX_GRID * MAX_GRID][2];
static int n_mirror_cells;

static void build_sat(struct solver *S) {
  int i, j, d, k;
  solver_ok = 1;

  /* --- Section 1: walls, lasers, cats — fix known values --- */
  for (i = 0; i < gH; i++) {
    for (j = 0; j < gW; j++) {
      char c = grid[i][j];
      if (c == '#') {
        for (d = 0; d < 4; d++)
          clause1(S, -var_R(i, j, d));
      } else if (c == 'A' || c == 'V' || c == '<' || c == '>') {
        clause1(S, -var_M(i, j));
      } else if (c == 'O') {
        clause1(S, -var_M(i, j));
      }
    }
  }

  /* --- Section 2: laser sources — force R=true on adjacent cell --- */
  for (i = 0; i < gH; i++) {
    for (j = 0; j < gW; j++) {
      char c = grid[i][j];
      if (c == '>' && inside(i, j + 1))
        clause1(S, var_R(i, j + 1, DE));
      else if (c == '<' && inside(i, j - 1))
        clause1(S, var_R(i, j - 1, DW));
      else if (c == 'A' && inside(i - 1, j))
        clause1(S, var_R(i - 1, j, DN));
      else if (c == 'V' && inside(i + 1, j))
        clause1(S, var_R(i + 1, j, DS));
    }
  }

  /* --- Section 3: forward propagation --- */
  for (i = 0; i < gH; i++) {
    for (j = 0; j < gW; j++) {
      if (is_blocking(i, j))
        continue;
      int m = var_M(i, j), t = var_T(i, j);

      /* no mirror → light continues straight */
      for (d = 0; d < 4; d++) {
        int ni = i + dx[d], nj = j + dy[d];
        if (inside(ni, nj) && !is_blocking(ni, nj))
          clause3(S, -var_R(i, j, d), m, var_R(ni, nj, d));
      }
      /* "/" mirror (T=0): reflect and forward */
      for (k = 0; k < 4; k++) {
        int d_in = slash_fwd[k][0], di = slash_fwd[k][1];
        int dj = slash_fwd[k][2], d_out = slash_fwd[k][3];
        int ni = i + di, nj = j + dj;
        if (inside(ni, nj) && !is_blocking(ni, nj))
          clause4(S, -var_R(i, j, d_in), -m, t, var_R(ni, nj, d_out));
      }
      /* "\" mirror (T=1): reflect and forward */
      for (k = 0; k < 4; k++) {
        int d_in = back_fwd[k][0], di = back_fwd[k][1];
        int dj = back_fwd[k][2], d_out = back_fwd[k][3];
        int ni = i + di, nj = j + dj;
        if (inside(ni, nj) && !is_blocking(ni, nj))
          clause4(S, -var_R(i, j, d_in), -m, -t, var_R(ni, nj, d_out));
      }
    }
  }

  /* --- Section 3b: reverse propagation (beam must have valid source) --- */
  for (i = 0; i < gH; i++) {
    for (j = 0; j < gW; j++) {
      if (is_blocking(i, j))
        continue;
      for (d = 0; d < 4; d++) {
        int pi = i + src_dx[d], pj = j + src_dy[d];
        /* Is the source cell a laser emitting in direction d? */
        if (inside(pi, pj) && grid[pi][pj] == laser_for_dir(d))
          continue;
        /* No source → beam impossible from this direction */
        if (!inside(pi, pj) || is_blocking(pi, pj)) {
          clause1(S, -var_R(i, j, d));
          continue;
        }
        int pm = var_M(pi, pj), pt = var_T(pi, pj);
        /* no mirror on predecessor → must have come straight */
        clause3(S, -var_R(i, j, d), pm, var_R(pi, pj, d));
        /* "/" mirror on predecessor → must have come from slash_in_d[d] */
        clause4(S, -var_R(i, j, d), -pm, pt, var_R(pi, pj, slash_in_d[d]));
        /* "\" mirror on predecessor → must have come from back_in_d[d] */
        clause4(S, -var_R(i, j, d), -pm, -pt, var_R(pi, pj, back_in_d[d]));
      }
    }
  }

  /* --- Section 4: cats must be illuminated from at least one direction --- */
  for (i = 0; i < gH; i++)
    for (j = 0; j < gW; j++)
      if (grid[i][j] == 'O')
        clause4(S, var_R(i, j, DN), var_R(i, j, DS), var_R(i, j, DE),
                var_R(i, j, DW));

  /* --- Section 5: at-most-L mirrors (Sinz sequential counter) --- */
  if (gL == 0) {
    for (k = 0; k < n_mirror_cells; k++)
      clause1(S, -var_M(mirror_cells[k][0], mirror_cells[k][1]));
  } else if (n_mirror_cells > 0) {
    int x1 = var_M(mirror_cells[0][0], mirror_cells[0][1]);
    clause2(S, -x1, var_SC(1, 1));
    for (j = 2; j <= gL; j++)
      clause1(S, -var_SC(1, j));

    for (k = 2; k <= n_mirror_cells; k++) {
      int xk = var_M(mirror_cells[k - 1][0], mirror_cells[k - 1][1]);
      clause2(S, -xk, var_SC(k, 1));
      clause2(S, -var_SC(k - 1, 1), var_SC(k, 1));
      for (j = 2; j <= gL; j++) {
        clause3(S, -xk, -var_SC(k - 1, j - 1), var_SC(k, j));
        clause2(S, -var_SC(k - 1, j), var_SC(k, j));
      }
      clause2(S, -xk, -var_SC(k - 1, gL));
    }
  }
}

/* =========================================================================
   Part 4: Light simulation and verification
   ========================================================================= */

static char result[MAX_GRID][MAX_GRID];
static int lit[MAX_GRID][MAX_GRID]; /* bitmask: which directions illuminate cell */

static void trace_light(int si, int sj, int sd) {
  int ci = si, cj = sj, cd = sd;
  while (inside(ci, cj) && !is_blocking(ci, cj)) {
    if (lit[ci][cj] & (1 << cd)) break; /* cycle detected — stop */
    lit[ci][cj] |= (1 << cd);
    char cell = result[ci][cj];
    if (cell == '/') {
      if      (cd == DE) cd = DN;
      else if (cd == DN) cd = DE;
      else if (cd == DW) cd = DS;
      else               cd = DW;
    } else if (cell == '\\') {
      if      (cd == DE) cd = DS;
      else if (cd == DS) cd = DE;
      else if (cd == DW) cd = DN;
      else               cd = DW;
    }
    ci += dx[cd]; cj += dy[cd];
  }
}

static int verify(void) {
  int i, j;
  for (i = 0; i < gH; i++)
    for (j = 0; j < gW; j++)
      lit[i][j] = 0;

  /* Trace beams from every laser */
  for (i = 0; i < gH; i++)
    for (j = 0; j < gW; j++) {
      int d = -1;
      if      (grid[i][j] == '>') d = DE;
      else if (grid[i][j] == '<') d = DW;
      else if (grid[i][j] == 'A') d = DN;
      else if (grid[i][j] == 'V') d = DS;
      if (d < 0) continue;
      trace_light(i + dx[d], j + dy[d], d);
    }

  /* Check that every cat is illuminated */
  for (i = 0; i < gH; i++)
    for (j = 0; j < gW; j++)
      if (grid[i][j] == 'O' && lit[i][j] == 0) {
        fprintf(stderr, "VERIFY FAIL: cat at (%d,%d) not lit\n", i, j);
        return 0;
      }
  return 1;
}

/* =========================================================================
   Part 5: No-good clause storage for phantom cycle elimination
   ========================================================================= */

#define MAX_NOGOODS 200
#define MAX_NOGOOD_LITS 10000

static int nogood_clauses[MAX_NOGOODS][MAX_GRID * MAX_GRID + 1];
static int nogood_sizes[MAX_NOGOODS];
static int n_nogoods;

/* After verify fail: find phantom mirrors (M=1 but not on any real light path)
   and add a no-good clause blocking that mirror placement */
static void add_nogood(struct solver *S) {
  int buf[MAX_GRID * MAX_GRID];
  int sz = 0;
  int i, j;
  for (i = 0; i < gH; i++)
    for (j = 0; j < gW; j++)
      if (grid[i][j] == '.' && S->model[var_M(i, j)]) {
        /* Mirror placed — is it on a real light path? */
        if (lit[i][j] == 0) {
          /* Phantom mirror — not on any real light path */
          buf[sz++] = -var_M(i, j);
        }
      }
  if (sz == 0) {
    /* All mirrors are on real paths but cats still unlit —
       block the entire mirror configuration */
    for (i = 0; i < gH; i++)
      for (j = 0; j < gW; j++)
        if (grid[i][j] == '.') {
          if (S->model[var_M(i, j)])
            buf[sz++] = -var_M(i, j);
          else
            buf[sz++] = var_M(i, j);
        }
  }
  if (n_nogoods < MAX_NOGOODS && sz > 0) {
    nogood_sizes[n_nogoods] = sz;
    for (i = 0; i < sz; i++)
      nogood_clauses[n_nogoods][i] = buf[i];
    n_nogoods++;
  }
}

static void add_nogoods_to_solver(struct solver *S) {
  int i;
  for (i = 0; i < n_nogoods; i++)
    add_clause(S, nogood_clauses[i], nogood_sizes[i]);
}

/* =========================================================================
   Part 6: main — parse input, solve loop with verification, output
   ========================================================================= */

int main(void) {
  int i, j;

  /* Read the grid */
  scanf("%d %d %d", &gW, &gH, &gL);
  for (i = 0; i < gH; i++)
    scanf("%s", grid[i]);

  /* Collect empty cells (candidate mirror positions) */
  n_mirror_cells = 0;
  for (i = 0; i < gH; i++)
    for (j = 0; j < gW; j++)
      if (grid[i][j] == '.') {
        mirror_cells[n_mirror_cells][0] = i;
        mirror_cells[n_mirror_cells][1] = j;
        n_mirror_cells++;
      }

  /* Compute SC base and variable/clause counts */
  sc_base = 1 + 6 * gH * gW;
  int nVars = 6 * gH * gW + n_mirror_cells * (gL > 0 ? gL : 1);
  int nClauses = gH * gW * 40 + n_mirror_cells * gL * 4 + 1000;

  n_nogoods = 0;
  int max_iters = 100;

  for (int iter = 0; iter < max_iters; iter++) {
    /* (Re)initialize solver */
    struct solver S;
    int extra = n_nogoods * (n_mirror_cells + 2);
    initCDCL(&S, nVars, nClauses + extra);
    build_sat(&S);
    add_nogoods_to_solver(&S);

    if (!solver_ok || solve(&S) == UNSAT) {
      printf("UNSAT\n");
      free(S.DB - 0); /* DB was malloced at offset 0 */
      return 0;
    }

    /* Build result grid from SAT model */
    for (i = 0; i < gH; i++)
      for (j = 0; j < gW; j++) {
        if (grid[i][j] == '.' && S.model[var_M(i, j)])
          result[i][j] = S.model[var_T(i, j)] ? '\\' : '/';
        else
          result[i][j] = grid[i][j];
      }

    /* Verify by simulating real light paths */
    if (verify()) {
      /* Success — print solution */
      printf("%d %d %d\n", gW, gH, gL);
      for (i = 0; i < gH; i++) {
        for (j = 0; j < gW; j++)
          putchar(result[i][j]);
        putchar('\n');
      }
      free(S.DB);
      return 0;
    }

    /* Phantom cycle detected — add no-good clause and retry */
    add_nogood(&S);
    free(S.DB);
  }

  printf("UNSAT\n");
  return 0;
}
