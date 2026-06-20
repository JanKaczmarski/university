#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_DIM 105
#define MAX_PATH_LEN 20000
#define MAX_MIRRORS_LIMIT 205

typedef struct
{
    int x;
    int y;
} Point;

typedef struct
{
    int x;
    int y;
    char dir;
} PathState;

typedef struct
{
    int x;
    int y;
} TargetPos;

/* -------------------------------------------------------------------------
 * Kandydat na lustro: wynik heurystyki + pozycja + typ lustra
 * ------------------------------------------------------------------------- */
typedef struct
{
    int  score;       /* liczba kotów oświetlonych po tym ruchu */
    int  path_idx;    /* indeks w path_after[] */
    char mirror_type; /* '/' lub '\\' */
} Candidate;

static int cmp_candidates_desc(const void *a, const void *b)
{
    return ((Candidate *)b)->score - ((Candidate *)a)->score;
}

/* Globalny/statyczny bufor na ścieżki dla poszczególnych głębokości rekurencji */
static PathState global_path_pool[MAX_MIRRORS_LIMIT][MAX_PATH_LEN];

/* Osobny bufor używany tylko podczas oceniania kandydatów (nie wchodzi w rekurencję) */
static PathState score_path_buf[MAX_PATH_LEN];

/* Tablica preprocessingu: [y][x][kierunek: N=0, S=1, W=2, E=3] */
static TargetPos next_interesting[MAX_DIM][MAX_DIM][4];

int get_dx(char dir)
{
    if (dir == 'E') return 1;
    if (dir == 'W') return -1;
    return 0;
}

int get_dy(char dir)
{
    if (dir == 'S') return 1;
    if (dir == 'N') return -1;
    return 0;
}

int dir_to_idx(char dir)
{
    if (dir == 'N') return 0;
    if (dir == 'S') return 1;
    if (dir == 'W') return 2;
    return 3; /* 'E' */
}

char reflect(char mirror, char dir)
{
    if (mirror == '/')
    {
        if (dir == 'N') return 'E';
        if (dir == 'S') return 'W';
        if (dir == 'E') return 'N';
        if (dir == 'W') return 'S';
    }
    else if (mirror == '\\')
    {
        if (dir == 'N') return 'W';
        if (dir == 'S') return 'E';
        if (dir == 'E') return 'S';
        if (dir == 'W') return 'N';
    }
    return dir;
}

bool contains_cat(Point *cats, int count, int x, int y)
{
    for (int i = 0; i < count; i++)
        if (cats[i].x == x && cats[i].y == y)
            return true;
    return false;
}

void compute_preprocessing(int W, int H, char grid[MAX_DIM][MAX_DIM])
{
    char dirs[4] = {'N', 'S', 'W', 'E'};
    for (int y = 0; y < H; y++)
    {
        for (int x = 0; x < W; x++)
        {
            for (int d = 0; d < 4; d++)
            {
                char dir = dirs[d];
                int dx = get_dx(dir);
                int dy = get_dy(dir);
                int cx = x + dx;
                int cy = y + dy;

                while (cx >= 0 && cx < W && cy >= 0 && cy < H)
                {
                    if (grid[cy][cx] != '.')
                        break;
                    cx += dx;
                    cy += dy;
                }
                next_interesting[y][x][d].x = cx;
                next_interesting[y][x][d].y = cy;
            }
        }
    }
}

int simulate(int W, int H, char grid[MAX_DIM][MAX_DIM],
             int start_x, int start_y, char start_dir,
             PathState *path_after)
{
    int x = start_x;
    int y = start_y;
    char d = start_dir;

    static bool visited[MAX_DIM][MAX_DIM][4];
    memset(visited, 0, sizeof(visited));

    Point illuminated_cats[MAX_DIM * MAX_DIM];
    int ill_count = 0;
    int path_cnt = 0;

    while (x >= 0 && x < W && y >= 0 && y < H)
    {
        int d_idx = dir_to_idx(d);

        if (visited[y][x][d_idx])
            break;
        visited[y][x][d_idx] = true;

        char current_char = grid[y][x];

        if (current_char == '#')
            break;

        if (current_char == 'O')
        {
            if (!contains_cat(illuminated_cats, ill_count, x, y))
            {
                illuminated_cats[ill_count].x = x;
                illuminated_cats[ill_count].y = y;
                ill_count++;
            }
        }

        if (current_char == '/' || current_char == '\\')
        {
            d = reflect(current_char, d);
            path_cnt = 0;
            d_idx = dir_to_idx(d);
        }

        path_after[path_cnt].x = x;
        path_after[path_cnt].y = y;
        path_after[path_cnt].dir = d;
        path_cnt++;

        /* --- SKOK OPTYMALIZACYJNY --- */
        TargetPos target = next_interesting[y][x][d_idx];
        int dx = get_dx(d);
        int dy = get_dy(d);

        int next_x = x + dx;
        int next_y = y + dy;

        bool mirror_found = false;
        while (next_x != target.x || next_y != target.y)
        {
            if (grid[next_y][next_x] == '/' || grid[next_y][next_x] == '\\')
            {
                mirror_found = true;
                break;
            }

            path_after[path_cnt].x = next_x;
            path_after[path_cnt].y = next_y;
            path_after[path_cnt].dir = d;
            path_cnt++;

            next_x += dx;
            next_y += dy;
        }

        x = mirror_found ? next_x : target.x;
        y = mirror_found ? next_y : target.y;
    }

    return (ill_count << 16) | (path_cnt & 0xFFFF);
}

bool solve_dfs(int W, int H, int max_mirrors, char grid[MAX_DIM][MAX_DIM],
               int start_x, int start_y, char start_dir,
               int total_cats, int mirrors_placed, int prev_cats_count)
{
    PathState *path_after = global_path_pool[mirrors_placed];

    int sim_res = simulate(W, H, grid, start_x, start_y, start_dir, path_after);
    int illuminated_count = sim_res >> 16;
    int path_count        = sim_res & 0xFFFF;

    if (illuminated_count < prev_cats_count)
        return false;

    if (illuminated_count == total_cats)
        return true;

    if (mirrors_placed >= max_mirrors)
        return false;

    /* -----------------------------------------------------------------------
     * Heurystyka: oceniamy każdy kandydat (pozycja × typ lustra) przez
     * tymczasowe postawienie lustra i symulację. Sortujemy malejąco
     * po liczbie oświetlonych kotów — najpierw próbujemy najbardziej
     * obiecujące ruchy.
     * --------------------------------------------------------------------- */
    static Candidate candidates[MAX_PATH_LEN * 2];
    int n_candidates = 0;

    for (int i = 0; i < path_count; i++)
    {
        int px = path_after[i].x;
        int py = path_after[i].y;
        char incoming_dir = path_after[i].dir;

        if (grid[py][px] != '.')
            continue;

        char mirror_types[2] = {'/', '\\'};
        for (int m = 0; m < 2; m++)
        {
            char m_type = mirror_types[m];

            /* --- GEOMETRYCZNE ODCINANIE --- */
            char new_dir = reflect(m_type, incoming_dir);
            int ndx = get_dx(new_dir);
            int ndy = get_dy(new_dir);
            int n_idx = dir_to_idx(new_dir);

            int immediate_x = px + ndx;
            int immediate_y = py + ndy;

            TargetPos target = next_interesting[py][px][n_idx];

            if (immediate_x < 0 || immediate_x >= W ||
                immediate_y < 0 || immediate_y >= H ||
                (target.x == immediate_x && target.y == immediate_y &&
                 grid[immediate_y][immediate_x] == '#'))
            {
                continue;
            }
            /* ----------------------------- */

            /* Oceniamy: ile kotów po postawieniu tego lustra? */
            grid[py][px] = m_type;
            int score_res = simulate(W, H, grid, start_x, start_y, start_dir,
                                     score_path_buf);
            int score = score_res >> 16;
            grid[py][px] = '.';

            candidates[n_candidates].score       = score;
            candidates[n_candidates].path_idx    = i;
            candidates[n_candidates].mirror_type = m_type;
            n_candidates++;
        }
    }

    /* Sortuj malejąco po score */
    qsort(candidates, n_candidates, sizeof(Candidate), cmp_candidates_desc);

    /* Iteruj po posortowanych kandydatach */
    for (int c = 0; c < n_candidates; c++)
    {
        int i     = candidates[c].path_idx;
        int px    = path_after[i].x;
        int py    = path_after[i].y;
        char m_type = candidates[c].mirror_type;

        grid[py][px] = m_type;

        if (solve_dfs(W, H, max_mirrors, grid, start_x, start_y, start_dir,
                      total_cats, mirrors_placed + 1, illuminated_count))
            return true;

        grid[py][px] = '.';
    }

    return false;
}

int main(void)
{
    int W, H, L;
    if (scanf("%d %d %d", &W, &H, &L) != 3)
        return 0;

    char grid[MAX_DIM][MAX_DIM];
    int start_x = -1, start_y = -1;
    char start_dir = ' ';
    int total_cats = 0;

    for (int y = 0; y < H; y++)
    {
        scanf("%s", grid[y]);
        for (int x = 0; x < W; x++)
        {
            char c = grid[y][x];
            if      (c == 'A') { start_x = x; start_y = y; start_dir = 'N'; }
            else if (c == 'V') { start_x = x; start_y = y; start_dir = 'S'; }
            else if (c == '<') { start_x = x; start_y = y; start_dir = 'W'; }
            else if (c == '>') { start_x = x; start_y = y; start_dir = 'E'; }
            else if (c == 'O') { total_cats++; }
        }
    }

    if (start_x != -1)
    {
        compute_preprocessing(W, H, grid);
        solve_dfs(W, H, L, grid, start_x, start_y, start_dir,
                  total_cats, 0, 0);
    }

    printf("%d %d %d\n", W, H, L);
    for (int y = 0; y < H; y++)
        printf("%s\n", grid[y]);

    return 0;
}
