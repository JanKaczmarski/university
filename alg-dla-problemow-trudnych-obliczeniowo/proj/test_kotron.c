/*
 * test_kotron.c — Test harness for the Kotron SAT solver.
 *
 * Compiles and runs the solver (main.c) on a set of test cases,
 * then verifies the output: correct format, mirror count within limit,
 * and all cats illuminated by real light simulation.
 *
 * Build:  make test_kotron
 * Run:    make test
 *
 * Requires: main.c compiled as ./build/kotron.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_GRID 101
#define DN 0
#define DS 1
#define DE 2
#define DW 3

static const int dx[4] = {-1, 1, 0, 0};
static const int dy[4] = {0, 0, 1, -1};

static int inside(int i, int j, int H, int W) {
  return i >= 0 && i < H && j >= 0 && j < W;
}

/* Simulate light and check all cats are lit. Returns 1 if valid. */
static int verify_solution(int W, int H, int L,
                           const char grid[MAX_GRID][MAX_GRID],
                           const char sol[MAX_GRID][MAX_GRID]) {
  int lit[MAX_GRID][MAX_GRID];
  int i, j;
  memset(lit, 0, sizeof(lit));

  /* Check: solution preserves non-empty cells from input */
  for (i = 0; i < H; i++)
    for (j = 0; j < W; j++) {
      char g = grid[i][j], s = sol[i][j];
      if (g != '.') {
        if (s != g) {
          fprintf(stderr,
                  "  FAIL: cell (%d,%d) changed from '%c' to '%c'\n", i, j, g,
                  s);
          return 0;
        }
      } else {
        if (s != '.' && s != '/' && s != '\\') {
          fprintf(stderr,
                  "  FAIL: cell (%d,%d) has invalid char '%c'\n", i, j, s);
          return 0;
        }
      }
    }

  /* Count mirrors */
  int mirror_count = 0;
  for (i = 0; i < H; i++)
    for (j = 0; j < W; j++)
      if (grid[i][j] == '.' && (sol[i][j] == '/' || sol[i][j] == '\\'))
        mirror_count++;

  if (mirror_count > L) {
    fprintf(stderr, "  FAIL: %d mirrors placed, limit is %d\n", mirror_count,
            L);
    return 0;
  }

  /* Trace light from each laser */
  for (i = 0; i < H; i++)
    for (j = 0; j < W; j++) {
      int d = -1;
      if (grid[i][j] == '>')
        d = DE;
      else if (grid[i][j] == '<')
        d = DW;
      else if (grid[i][j] == 'A')
        d = DN;
      else if (grid[i][j] == 'V')
        d = DS;
      if (d < 0)
        continue;

      int ci = i + dx[d], cj = j + dy[d], cd = d;
      while (inside(ci, cj, H, W) && sol[ci][cj] != '#') {
        if (lit[ci][cj] & (1 << cd))
          break; /* cycle */
        lit[ci][cj] |= (1 << cd);
        char cell = sol[ci][cj];
        if (cell == '/') {
          if (cd == DE)
            cd = DN;
          else if (cd == DN)
            cd = DE;
          else if (cd == DW)
            cd = DS;
          else
            cd = DW;
        } else if (cell == '\\') {
          if (cd == DE)
            cd = DS;
          else if (cd == DS)
            cd = DE;
          else if (cd == DW)
            cd = DN;
          else
            cd = DW;
        }
        ci += dx[cd];
        cj += dy[cd];
      }
    }

  /* Check all cats are lit */
  for (i = 0; i < H; i++)
    for (j = 0; j < W; j++)
      if (grid[i][j] == 'O' && lit[i][j] == 0) {
        fprintf(stderr, "  FAIL: cat at (%d,%d) not illuminated\n", i, j);
        return 0;
      }

  return 1;
}

/* Run solver on input string, parse output, verify. Returns 1 if pass. */
static int run_test(const char *name, const char *input, int expect_sat) {
  printf("Test %-30s ... ", name);
  fflush(stdout);

  /* Write input to temp file */
  FILE *f = fopen("/tmp/kotron_test_in.txt", "w");
  if (!f) {
    printf("FAIL (cannot create temp file)\n");
    return 0;
  }
  fputs(input, f);
  fclose(f);

  /* Run solver */
  FILE *p = popen("./build/kotron < /tmp/kotron_test_in.txt 2>/dev/null", "r");
  if (!p) {
    printf("FAIL (cannot run solver)\n");
    return 0;
  }

  char output[MAX_GRID * MAX_GRID + 1000];
  int len = 0;
  while (len < (int)sizeof(output) - 1) {
    int c = fgetc(p);
    if (c == EOF)
      break;
    output[len++] = c;
  }
  output[len] = '\0';
  pclose(p);

  /* Check for UNSAT */
  if (strncmp(output, "UNSAT", 5) == 0) {
    if (!expect_sat) {
      printf("PASS (UNSAT as expected)\n");
      return 1;
    } else {
      printf("FAIL (got UNSAT, expected SAT)\n");
      return 0;
    }
  }

  if (!expect_sat) {
    printf("FAIL (got SAT, expected UNSAT)\n");
    return 0;
  }

  /* Parse output: W H L then H rows */
  int oW, oH, oL;
  char sol[MAX_GRID][MAX_GRID];
  int n = sscanf(output, "%d %d %d", &oW, &oH, &oL);
  if (n != 3) {
    printf("FAIL (cannot parse header)\n");
    return 0;
  }

  /* Parse grid from input */
  int iW, iH, iL;
  char grid[MAX_GRID][MAX_GRID];
  const char *ptr = input;
  sscanf(ptr, "%d %d %d", &iW, &iH, &iL);
  /* skip to first newline */
  while (*ptr && *ptr != '\n')
    ptr++;
  if (*ptr)
    ptr++;
  for (int i = 0; i < iH; i++) {
    int j = 0;
    while (*ptr && *ptr != '\n' && j < iW) {
      grid[i][j++] = *ptr++;
    }
    grid[i][j] = '\0';
    if (*ptr == '\n')
      ptr++;
  }

  /* Parse solution grid */
  const char *optr = output;
  while (*optr && *optr != '\n')
    optr++;
  if (*optr)
    optr++;
  for (int i = 0; i < oH; i++) {
    int j = 0;
    while (*optr && *optr != '\n' && j < oW) {
      sol[i][j++] = *optr++;
    }
    sol[i][j] = '\0';
    if (*optr == '\n')
      optr++;
  }

  if (oW != iW || oH != iH) {
    printf("FAIL (dimension mismatch: %dx%d vs %dx%d)\n", oW, oH, iW, iH);
    return 0;
  }

  if (!verify_solution(iW, iH, iL, grid, sol)) {
    printf("FAIL (invalid solution)\n");
    return 0;
  }

  printf("PASS\n");
  return 1;
}

int main(void) {
  int passed = 0, total = 0;

  /* ---- Test 1: Problem statement example (12x7, L=3) ---- */
  total++;
  passed += run_test("problem_example_12x7", "12 7 3\n"
                                              "............\n"
                                              "..>..O......\n"
                                              ".........###\n"
                                              "O..........O\n"
                                              ".........###\n"
                                              "..>.O.......\n"
                                              "............\n",
                      1);

  /* ---- Test 2: Small grid with pre-placed mirrors ---- */
  total++;
  passed += run_test("prebuilt_6x6", "6 6 3\n"
                                      ">.\\...\n"
                                      "......\n"
                                      "..O...\n"
                                      "..\\O..\n"
                                      "......\n"
                                      "../..<\n",
                      1);

  /* ---- Test 3: Trivial — single laser, single cat, direct line ---- */
  total++;
  passed += run_test("direct_line", "3 1 0\n"
                                     ">O.\n",
                      1);

  /* ---- Test 4: Single laser, one mirror needed ---- */
  total++;
  passed += run_test("one_mirror_needed", "3 3 1\n"
                                           ">..\n"
                                           "...\n"
                                           "..O\n",
                      1);

  /* ---- Test 5: UNSAT — cat fully surrounded by walls ---- */
  total++;
  passed += run_test("unsat_wall_blocks", "3 3 2\n"
                                           ">..\n"
                                           ".#.\n"
                                           "#O#\n",
                      0);

  /* ---- Test 6: Zero mirrors allowed, cat already in line ---- */
  total++;
  passed += run_test("zero_mirrors_inline", "5 1 0\n"
                                             ">..O.\n",
                      1);

  /* ---- Test 7: Zero mirrors allowed, cat NOT in line → UNSAT ---- */
  total++;
  passed += run_test("zero_mirrors_unsat", "3 3 0\n"
                                            ">..\n"
                                            "...\n"
                                            "..O\n",
                      0);

  /* ---- Test 8: Multiple cats, one laser, requires 2 mirrors ---- */
  total++;
  passed += run_test("two_cats_two_mirrors", "5 5 2\n"
                                              ">.O..\n"
                                              ".....\n"
                                              ".....\n"
                                              ".....\n"
                                              "O....\n",
                      1);

  /* ---- Test 9: Cat on laser path (transparent) ---- */
  total++;
  passed += run_test("cat_transparent", "5 1 0\n"
                                         ">OOOO\n",
                      1);

  /* ---- Test 10: Laser aiming at boundary ---- */
  total++;
  passed += run_test("laser_at_boundary", "3 1 0\n"
                                           "..>\n",
                      1);

  /* ---- Test 11: Two lasers, two cats ---- */
  total++;
  passed += run_test("two_lasers_two_cats", "5 3 1\n"
                                             ">.O..\n"
                                             ".....\n"
                                             "..O.<\n",
                      1);

  /* ---- Test 12: 1x1 grid with just a dot (no cat) ---- */
  total++;
  passed += run_test("trivial_1x1_dot", "1 1 0\n"
                                          ".\n",
                      1);

  /* ---- Test 13: Laser pointing off-grid ---- */
  total++;
  passed += run_test("laser_offgrid", "1 1 0\n"
                                       ">\n",
                      1);

  /* ---- Test 14: Multiple laser directions pointing toward cat ---- */
  total++;
  passed += run_test("four_direction_lasers", "5 5 0\n"
                                               "..V..\n"
                                               ".....\n"
                                               ">.O.<\n"
                                               ".....\n"
                                               "..A..\n",
                      1);

  /* ---- Test 15: Beam through laser cell (lasers are transparent) ---- */
  total++;
  passed += run_test("beam_through_laser", "5 1 0\n"
                                            ">>.O.\n",
                      1);

  printf("\n=== Results: %d/%d passed ===\n", passed, total);
  return passed == total ? 0 : 1;
}
