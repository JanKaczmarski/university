#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <limits.h>

#define INF 1000000000

typedef struct { int r, c, d; } Laser;
typedef struct { int *a; int n, cap; } IntVec;
typedef struct { unsigned char *lit; int litCount; int *entries; int entryCount; } Sim;
typedef struct { int *dist, *parent, *pcode; } BFSData;
typedef struct { int ok; IntVec codes; IntVec states; int endState; int cost; int newLit; } RouteRes;
typedef struct { int dist, pos, id; } Reach;
typedef struct {
    int *codes;
    int codesN;
    unsigned char *lit;
    unsigned char *forb;
    int cur;
    int litCount;
    int score;
} PState;

static int W,H,L,N,N4;
static int L_input;
static char *orig;
static int *catId;
static int *catCell;
static int C;
static Laser *lasers;
static int laserCount;
static int dr[4] = {-1,0,1,0};
static int dc[4] = {0,1,0,-1};
static int refl[2][4];
static unsigned short *seenStamp;
static unsigned short seenIter = 1;
static unsigned short *tmpCellStamp;
static unsigned char *tmpCellOri;
static unsigned short tmpStamp = 1;
static int *rowWall, *rowCat, *colWall, *colCat;
static double globalDeadline;

static double now_sec(void) {
    return (double)clock() / (double)CLOCKS_PER_SEC;
}

static int timeExpired(void) {
    return now_sec() > globalDeadline;
}

static int cell_of(int r, int c) { return r*W + c; }
static int inside(int r, int c) { return r>=0 && r<H && c>=0 && c<W; }
static char ori_char(int ori) { return ori==0 ? '/' : '\\'; }

static void ensure_seen_or_die(void) {
    if (!seenStamp) {
        seenStamp = (unsigned short*)calloc((size_t)(N4>0?N4:1), sizeof(unsigned short));
        if (!seenStamp) exit(2);
        seenIter = 1;
    }
}

static void ensure_tmp_or_die(void) {
    if (!tmpCellStamp) {
        tmpCellStamp = (unsigned short*)calloc((size_t)(N>0?N:1), sizeof(unsigned short));
        tmpCellOri = (unsigned char*)malloc(sizeof(unsigned char)*(size_t)(N>0?N:1));
        if (!tmpCellStamp || !tmpCellOri) exit(2);
        tmpStamp = 1;
    }
}

static int dir_from_char(char ch) {
    if (ch=='A') return 0;
    if (ch=='>') return 1;
    if (ch=='V') return 2;
    if (ch=='<') return 3;
    return -1;
}

static void vec_init(IntVec *v) { v->a=NULL; v->n=0; v->cap=0; }
static void vec_free(IntVec *v) { free(v->a); v->a=NULL; v->n=0; v->cap=0; }
static void vec_reserve(IntVec *v, int cap) {
    if (cap <= v->cap) return;
    int nc = v->cap ? v->cap : 8;
    while (nc < cap) nc *= 2;
    int *na = (int*)realloc(v->a, sizeof(int)*nc);
    if (!na) exit(2);
    v->a = na; v->cap = nc;
}
static void vec_push(IntVec *v, int x) {
    if (v->n >= v->cap) vec_reserve(v, v->n+1);
    v->a[v->n++] = x;
}
static void vec_copy(IntVec *dst, const IntVec *src) {
    vec_init(dst);
    vec_reserve(dst, src->n);
    if (src->n) memcpy(dst->a, src->a, sizeof(int)*src->n);
    dst->n = src->n;
}

static int cmp_int_asc(const void *a, const void *b) {
    int x=*(const int*)a, y=*(const int*)b;
    return (x>y) - (x<y);
}

static void reverse_ints(int *a, int n) {
    int i=0,j=n-1;
    while (i<j) { int t=a[i]; a[i]=a[j]; a[j]=t; i++; j--; }
}

/* Dynamic deque for 0-1 BFS. */
typedef struct { int *a; int cap, head, size; } Deque;
static void dq_init(Deque *q, int cap) {
    if (cap < 16) cap = 16;
    q->a = (int*)malloc(sizeof(int)*cap);
    if (!q->a) exit(2);
    q->cap=cap; q->head=0; q->size=0;
}
static void dq_free(Deque *q) { free(q->a); q->a=NULL; q->cap=q->head=q->size=0; }
static void dq_grow(Deque *q) {
    int ncap = q->cap * 2;
    int *na = (int*)malloc(sizeof(int)*ncap);
    if (!na) exit(2);
    for (int i=0;i<q->size;i++) na[i] = q->a[(q->head+i)%q->cap];
    free(q->a); q->a=na; q->cap=ncap; q->head=0;
}
static void dq_push_front(Deque *q, int x) {
    if (q->size == q->cap) dq_grow(q);
    q->head = (q->head - 1 + q->cap) % q->cap;
    q->a[q->head]=x; q->size++;
}
static void dq_push_back(Deque *q, int x) {
    if (q->size == q->cap) dq_grow(q);
    int pos = (q->head + q->size) % q->cap;
    q->a[pos]=x; q->size++;
}
static int dq_pop_front(Deque *q) {
    int x=q->a[q->head];
    q->head = (q->head + 1) % q->cap;
    q->size--;
    return x;
}

static int build_prefixes(void) {
    rowWall = (int*)calloc((size_t)H*(W+1), sizeof(int));
    rowCat  = (int*)calloc((size_t)H*(W+1), sizeof(int));
    colWall = (int*)calloc((size_t)W*(H+1), sizeof(int));
    colCat  = (int*)calloc((size_t)W*(H+1), sizeof(int));
    if (!rowWall || !rowCat || !colWall || !colCat) {
        free(rowWall); free(rowCat); free(colWall); free(colCat);
        rowWall = rowCat = colWall = colCat = NULL;
        return 0;
    }
    for (int r=0;r<H;r++) {
        for (int c=0;c<W;c++) {
            rowWall[r*(W+1)+c+1] = rowWall[r*(W+1)+c] + (orig[cell_of(r,c)]=='#');
            rowCat [r*(W+1)+c+1] = rowCat [r*(W+1)+c] + (orig[cell_of(r,c)]=='O');
        }
    }
    for (int c=0;c<W;c++) {
        for (int r=0;r<H;r++) {
            colWall[c*(H+1)+r+1] = colWall[c*(H+1)+r] + (orig[cell_of(r,c)]=='#');
            colCat [c*(H+1)+r+1] = colCat [c*(H+1)+r] + (orig[cell_of(r,c)]=='O');
        }
    }
    return 1;
}

static int row_segment_no_wall(int r, int c1, int c2) {
    if (c1>c2) { int t=c1; c1=c2; c2=t; }
    return rowWall[r*(W+1)+c2+1] - rowWall[r*(W+1)+c1] == 0;
}
static int row_cats_covered_by_segment(int r, int c1, int c2) {
    if (c1>c2) { int t=c1; c1=c2; c2=t; }
    int total = rowCat[r*(W+1)+W];
    int insideCats = rowCat[r*(W+1)+c2+1] - rowCat[r*(W+1)+c1];
    return total == insideCats;
}
static int col_segment_no_wall(int c, int r1, int r2) {
    if (r1>r2) { int t=r1; r1=r2; r2=t; }
    return colWall[c*(H+1)+r2+1] - colWall[c*(H+1)+r1] == 0;
}
static int col_cats_covered_by_segment(int c, int r1, int r2) {
    if (r1>r2) { int t=r1; r1=r2; r2=t; }
    int total = colCat[c*(H+1)+H];
    int insideCats = colCat[c*(H+1)+r2+1] - colCat[c*(H+1)+r1];
    return total == insideCats;
}

static int read_input(void) {
    if (scanf("%d %d %d", &W, &H, &L) != 3) return 0;
    L_input = L;
    N = W*H; N4 = N*4;
    orig = (char*)malloc((size_t)(N>0?N:1));
    catId = NULL;
    seenStamp = NULL;
    tmpCellStamp = NULL;
    tmpCellOri = NULL;
    int catCap = 16, laserCap = 16;
    catCell = (int*)malloc(sizeof(int)*(size_t)catCap);
    lasers = (Laser*)malloc(sizeof(Laser)*(size_t)laserCap);
    if (!orig || !catCell || !lasers) exit(2);
    char *line = (char*)malloc((size_t)W + 8);
    if (!line) exit(2);
    C=0; laserCount=0;
    int emptyCount=0;
    for (int r=0;r<H;r++) {
        scanf("%s", line);
        for (int c=0;c<W;c++) {
            char ch = line[c];
            int ce = cell_of(r,c);
            orig[ce] = ch;
            int d = dir_from_char(ch);
            if (d >= 0) {
                if (laserCount >= laserCap) {
                    laserCap *= 2;
                    Laser *nl = (Laser*)realloc(lasers, sizeof(Laser)*(size_t)laserCap);
                    if (!nl) exit(2);
                    lasers = nl;
                }
                lasers[laserCount++] = (Laser){r,c,d};
            }
            if (ch == '.') emptyCount++;
            if (ch == 'O') {
                if (C >= catCap) {
                    catCap *= 2;
                    int *ncats = (int*)realloc(catCell, sizeof(int)*(size_t)catCap);
                    if (!ncats) exit(2);
                    catCell = ncats;
                }
                catCell[C++] = ce;
            }
        }
    }
    free(line);
    if (C > 0) {
        catId = (int*)malloc(sizeof(int)*(size_t)(N>0?N:1));
        if (!catId) exit(2);
        for (int i=0;i<N;i++) catId[i] = -1;
        for (int i=0;i<C;i++) catId[catCell[i]] = i;
    } else {
        catId = (int*)malloc(sizeof(int));
        if (!catId) exit(2);
        catId[0] = -1;
    }
    if (L < 0) L = 0;
    if (L > emptyCount) L = emptyCount;
    return 1;
}

static void simulate_into(const char *b, unsigned char *lit, int *entries, int *litCount, int *entryCount) {
    ensure_seen_or_die();
    if (C > 0) memset(lit, 0, (size_t)C);
    *litCount = 0; *entryCount = 0;
    seenIter++;
    if (seenIter == 0) { memset(seenStamp, 0, sizeof(unsigned short)*(size_t)N4); seenIter = 1; }
    for (int li=0; li<laserCount; li++) {
        int r=lasers[li].r, c=lasers[li].c, d=lasers[li].d;
        while (inside(r,c) && b[cell_of(r,c)] != '#') {
            int ce = cell_of(r,c);
            int st = (ce<<2) | d;
            if (seenStamp[st] == seenIter) break;
            seenStamp[st] = seenIter;
            entries[(*entryCount)++] = st;
            int id = catId[ce];
            if (id >= 0 && !lit[id]) { lit[id] = 1; (*litCount)++; }
            char ch = b[ce];
            if (ch == '/') d = refl[0][d];
            else if (ch == '\\') d = refl[1][d];
            r += dr[d]; c += dc[d];
        }
    }
}

static Sim sim_make(void) {
    Sim s;
    s.lit = (unsigned char*)malloc((size_t)(C>0?C:1));
    s.entries = (int*)malloc(sizeof(int)*(size_t)(N4>0?N4:1));
    if (!s.lit || !s.entries) exit(2);
    s.litCount=0; s.entryCount=0;
    return s;
}
static void sim_compute(Sim *s, const char *b) { simulate_into(b, s->lit, s->entries, &s->litCount, &s->entryCount); }
static void sim_free(Sim *s) { free(s->lit); free(s->entries); s->lit=NULL; s->entries=NULL; s->litCount=s->entryCount=0; }

static char *board_copy(const char *src) {
    char *b = (char*)malloc((size_t)N);
    if (!b) exit(2);
    memcpy(b, src, (size_t)N);
    return b;
}

static void board_from_codes(const int *codes, int codesN, char *out) {
    memcpy(out, orig, (size_t)N);
    for (int i=0;i<codesN;i++) {
        int code=codes[i], ce=code/2, ori=code&1;
        if (ce>=0 && ce<N && (out[ce]=='.' || out[ce]=='/' || out[ce]=='\\')) out[ce]=ori_char(ori);
    }
}

static int apply_codes_to_board(char *b, const IntVec *codes) {
    ensure_tmp_or_die();
    tmpStamp++;
    if (tmpStamp == 0) { memset(tmpCellStamp, 0, sizeof(unsigned short)*(size_t)N); tmpStamp=1; }
    for (int i=0;i<codes->n;i++) {
        int code=codes->a[i], ce=code/2, ori=code&1;
        if (ce<0 || ce>=N) return 0;
        if (tmpCellStamp[ce] == tmpStamp) {
            if (tmpCellOri[ce] != ori) return 0;
        } else {
            tmpCellStamp[ce] = tmpStamp;
            tmpCellOri[ce] = ori;
        }
    }
    for (int i=0;i<codes->n;i++) {
        int code=codes->a[i], ce=code/2, ori=code&1;
        char want = ori_char(ori);
        if (b[ce] != '.' && b[ce] != want) return 0;
    }
    for (int i=0;i<codes->n;i++) {
        int ce=codes->a[i]/2, ori=codes->a[i]&1;
        b[ce] = ori_char(ori);
    }
    return 1;
}

static int clean_codes_from_rev(const IntVec *rev, IntVec *clean) {
    ensure_tmp_or_die();
    vec_init(clean);
    tmpStamp++;
    if (tmpStamp == 0) { memset(tmpCellStamp,0,sizeof(unsigned short)*(size_t)N); tmpStamp=1; }
    for (int i=rev->n-1; i>=0; i--) {
        int code=rev->a[i], ce=code/2, ori=code&1;
        if (tmpCellStamp[ce] == tmpStamp) {
            if (tmpCellOri[ce] != ori) { vec_free(clean); return 0; }
            continue;
        }
        tmpCellStamp[ce]=tmpStamp; tmpCellOri[ce]=ori;
        vec_push(clean, code);
    }
    return 1;
}

static void bfs_alloc(BFSData *bd) {
    bd->dist = (int*)malloc(sizeof(int)*(size_t)N4);
    bd->parent = (int*)malloc(sizeof(int)*(size_t)N4);
    bd->pcode = (int*)malloc(sizeof(int)*(size_t)N4);
    if (!bd->dist || !bd->parent || !bd->pcode) exit(2);
}
static void bfs_free(BFSData *bd) { free(bd->dist); free(bd->parent); free(bd->pcode); bd->dist=bd->parent=bd->pcode=NULL; }

static void bfs_from_state(const char *b, int startState, int rem, const unsigned char *forbidden, BFSData *bd) {
    for (int i=0;i<N4;i++) { bd->dist[i]=INF; bd->parent[i]=-2; bd->pcode[i]=-1; }
    Deque dq; dq_init(&dq, N4 + 16);
    bd->dist[startState]=0; bd->parent[startState]=-1; dq_push_back(&dq, startState);
    long long pops=0;
    while (dq.size>0) {
        int st = dq_pop_front(&dq);
        if (((++pops) & 4095LL)==0 && timeExpired()) break;
        int base = bd->dist[st];
        if (base > rem) continue;
        int ce=st>>2, d=st&3;
        int r=ce/W, c=ce%W;
        int nr=r+dr[d], nc=c+dc[d];
        if (!inside(nr,nc) || b[cell_of(nr,nc)]=='#') continue;
        int ne=cell_of(nr,nc);
        char ch=b[ne];
        int targets[3], costs[3], codes[3], cnt=0;
        if (ch=='/') { targets[cnt]=(ne<<2)|refl[0][d]; costs[cnt]=base; codes[cnt++]=-1; }
        else if (ch=='\\') { targets[cnt]=(ne<<2)|refl[1][d]; costs[cnt]=base; codes[cnt++]=-1; }
        else if (ch=='.') {
            targets[cnt]=(ne<<2)|d; costs[cnt]=base; codes[cnt++]=-1;
            targets[cnt]=(ne<<2)|refl[0][d]; costs[cnt]=base+1; codes[cnt++]=ne*2+0;
            targets[cnt]=(ne<<2)|refl[1][d]; costs[cnt]=base+1; codes[cnt++]=ne*2+1;
        } else { targets[cnt]=(ne<<2)|d; costs[cnt]=base; codes[cnt++]=-1; }
        for (int i=0;i<cnt;i++) {
            int ns=targets[i], nd=costs[i], code=codes[i];
            if (nd > rem) continue;
            if (ns != startState && forbidden && forbidden[ns]) continue;
            if (nd < bd->dist[ns]) {
                bd->dist[ns]=nd; bd->parent[ns]=st; bd->pcode[ns]=code;
                if (code==-1) dq_push_front(&dq, ns); else dq_push_back(&dq, ns);
            }
        }
    }
    dq_free(&dq);
}

static int shortest_path_to_target(const char *b, const Sim *sim, int rem, int targetCat, IntVec *out) {
    vec_init(out);
    if (timeExpired()) return 0;
    int *dist=(int*)malloc(sizeof(int)*(size_t)N4);
    int *parent=(int*)malloc(sizeof(int)*(size_t)N4);
    int *pcode=(int*)malloc(sizeof(int)*(size_t)N4);
    if (!dist || !parent || !pcode) exit(2);
    for (int i=0;i<N4;i++) { dist[i]=INF; parent[i]=-2; pcode[i]=-1; }
    Deque dq; dq_init(&dq, N4+16);
    for (int ei=0; ei<sim->entryCount; ei++) {
        int en=sim->entries[ei], ce=en>>2, din=en&3;
        char ch=b[ce];
        int targets[3], costs[3], codes[3], cnt=0;
        if (ch=='#') continue;
        if (ch=='/') { targets[cnt]=(ce<<2)|refl[0][din]; costs[cnt]=0; codes[cnt++]=-1; }
        else if (ch=='\\') { targets[cnt]=(ce<<2)|refl[1][din]; costs[cnt]=0; codes[cnt++]=-1; }
        else if (ch=='.') {
            targets[cnt]=(ce<<2)|din; costs[cnt]=0; codes[cnt++]=-1;
            targets[cnt]=(ce<<2)|refl[0][din]; costs[cnt]=1; codes[cnt++]=ce*2+0;
            targets[cnt]=(ce<<2)|refl[1][din]; costs[cnt]=1; codes[cnt++]=ce*2+1;
        } else { targets[cnt]=(ce<<2)|din; costs[cnt]=0; codes[cnt++]=-1; }
        for (int i=0;i<cnt;i++) {
            int st=targets[i], nd=costs[i], code=codes[i];
            if (nd > rem) continue;
            if (nd < dist[st]) {
                dist[st]=nd; parent[st]=-1; pcode[st]=code;
                if (code==-1) dq_push_front(&dq, st); else dq_push_back(&dq, st);
            }
        }
    }
    int targetState=-1;
    long long pops=0;
    while (dq.size>0) {
        int st=dq_pop_front(&dq);
        if (((++pops)&4095LL)==0 && timeExpired()) break;
        int ce=st>>2, d=st&3;
        int id=catId[ce];
        int wanted = 0;
        if (targetCat >= 0) wanted = (id == targetCat);
        else wanted = (id >= 0 && !sim->lit[id]);
        if (wanted && dist[st] > 0) { targetState=st; break; }
        int r=ce/W, c=ce%W;
        int nr=r+dr[d], nc=c+dc[d];
        if (!inside(nr,nc) || b[cell_of(nr,nc)]=='#') continue;
        int ne=cell_of(nr,nc);
        char ch=b[ne];
        int base=dist[st];
        int targets[3], costs[3], codes[3], cnt=0;
        if (ch=='/') { targets[cnt]=(ne<<2)|refl[0][d]; costs[cnt]=base; codes[cnt++]=-1; }
        else if (ch=='\\') { targets[cnt]=(ne<<2)|refl[1][d]; costs[cnt]=base; codes[cnt++]=-1; }
        else if (ch=='.') {
            targets[cnt]=(ne<<2)|d; costs[cnt]=base; codes[cnt++]=-1;
            targets[cnt]=(ne<<2)|refl[0][d]; costs[cnt]=base+1; codes[cnt++]=ne*2+0;
            targets[cnt]=(ne<<2)|refl[1][d]; costs[cnt]=base+1; codes[cnt++]=ne*2+1;
        } else { targets[cnt]=(ne<<2)|d; costs[cnt]=base; codes[cnt++]=-1; }
        for (int i=0;i<cnt;i++) {
            int ns=targets[i], nd=costs[i], code=codes[i];
            if (nd > rem) continue;
            if (nd < dist[ns]) {
                dist[ns]=nd; parent[ns]=st; pcode[ns]=code;
                if (code==-1) dq_push_front(&dq, ns); else dq_push_back(&dq, ns);
            }
        }
    }
    dq_free(&dq);
    if (targetState < 0) { free(dist); free(parent); free(pcode); return 0; }
    IntVec rev; vec_init(&rev);
    int st=targetState;
    while (st!=-1 && st!=-2) {
        int code=pcode[st];
        if (code!=-1) vec_push(&rev, code);
        st=parent[st];
    }
    int ok = clean_codes_from_rev(&rev, out);
    vec_free(&rev);
    free(dist); free(parent); free(pcode);
    return ok && out->n > 0;
}

static int append_new_codes(IntVec *codes, const IntVec *add) {
    for (int i=0;i<add->n;i++) {
        int ce=add->a[i]/2, ori=add->a[i]&1;
        int found=0;
        for (int j=0;j<codes->n;j++) {
            if (codes->a[j]/2 == ce) {
                if ((codes->a[j]&1) != ori) return 0;
                found=1; break;
            }
        }
        if (!found) vec_push(codes, add->a[i]);
    }
    return 1;
}

static int greedy_constructive(char *answer) {
    char *b = board_copy(orig);
    IntVec codes; vec_init(&codes);
    Sim sim = sim_make();
    int *unlit=(int*)malloc(sizeof(int)*(size_t)(C>0?C:1));
    int *targets=(int*)malloc(sizeof(int)*(size_t)(C+2));
    if (!unlit || !targets) exit(2);
    for (int iter=0; iter<=L; iter++) {
        if (timeExpired()) break;
        sim_compute(&sim, b);
        if (sim.litCount == C) { memcpy(answer,b,(size_t)N); free(b); vec_free(&codes); sim_free(&sim); free(unlit); free(targets); return 1; }
        if (codes.n >= L) break;
        int rem = L - codes.n;
        int un=0;
        for (int i=0;i<C;i++) if (!sim.lit[i]) unlit[un++]=i;
        int tn=0;
        long long work = 1LL*un*(N>0?N:1);
        if (work <= 3000000LL) {
            for (int i=0;i<un;i++) targets[tn++]=unlit[i];
        } else {
            int take = un<80 ? un : 80;
            for (int i=0;i<take;i++) targets[tn++] = unlit[(long long)i*un/take];
            if (un>0) targets[tn++] = unlit[un-1];
            qsort(targets, tn, sizeof(int), cmp_int_asc);
            int m=0; for (int i=0;i<tn;i++) if (i==0 || targets[i]!=targets[i-1]) targets[m++]=targets[i]; tn=m;
        }
        IntVec bestAdd; vec_init(&bestAdd);
        int bestLit=-1, bestMirrors=INF, bestEntries=-1;
        for (int ti=0; ti<tn; ti++) {
            if (timeExpired()) break;
            IntVec add;
            if (!shortest_path_to_target(b, &sim, rem, targets[ti], &add)) { vec_free(&add); continue; }
            char *nb = board_copy(b);
            if (!apply_codes_to_board(nb, &add)) { free(nb); vec_free(&add); continue; }
            Sim ns = sim_make(); sim_compute(&ns, nb);
            if (ns.litCount > bestLit ||
                (ns.litCount==bestLit && add.n < bestMirrors) ||
                (ns.litCount==bestLit && add.n==bestMirrors && ns.entryCount > bestEntries)) {
                vec_free(&bestAdd);
                bestAdd = add;
                bestLit=ns.litCount; bestMirrors=add.n; bestEntries=ns.entryCount;
            } else vec_free(&add);
            sim_free(&ns); free(nb);
        }
        if (bestAdd.n == 0) {
            vec_free(&bestAdd);
            shortest_path_to_target(b, &sim, rem, -1, &bestAdd);
        }
        if (bestAdd.n == 0) { vec_free(&bestAdd); break; }
        char *nb = board_copy(b);
        if (!apply_codes_to_board(nb, &bestAdd)) { free(nb); vec_free(&bestAdd); break; }
        if (!append_new_codes(&codes, &bestAdd)) { free(nb); vec_free(&bestAdd); break; }
        free(b); b=nb; vec_free(&bestAdd);
    }
    sim_compute(&sim, b);
    int ok = (sim.litCount == C);
    if (ok) memcpy(answer,b,(size_t)N);
    free(b); vec_free(&codes); sim_free(&sim); free(unlit); free(targets);
    return ok;
}

static int dir_between_cells(int a, int b) {
    int ar=a/W, ac=a%W, br=b/W, bc=b%W;
    if (br==ar-1 && bc==ac) return 0;
    if (br==ar && bc==ac+1) return 1;
    if (br==ar+1 && bc==ac) return 2;
    if (br==ar && bc==ac-1) return 3;
    return -1;
}

static int try_laser_path_suffix(const IntVec *path, int idx, char *answer) {
    if (laserCount != 1) return 0;
    if (idx < 0 || idx+1 >= path->n) return 0;
    int firstDir = dir_between_cells(path->a[idx], path->a[idx+1]);
    if (firstDir != lasers[0].d) return 0;
    char *b = board_copy(orig);
    int used=0;
    for (int i=idx; i<path->n; i++) if (b[path->a[i]]=='#') { free(b); return 0; }
    for (int i=idx+1; i+1<path->n; i++) {
        int cur=path->a[i];
        int din=dir_between_cells(path->a[i-1], cur);
        int dout=dir_between_cells(cur, path->a[i+1]);
        if (din<0 || dout<0) { free(b); return 0; }
        if (din==dout) continue;
        int ori=-1;
        if (refl[0][din]==dout) ori=0;
        else if (refl[1][din]==dout) ori=1;
        else { free(b); return 0; }
        if (b[cur] != '.') { free(b); return 0; }
        b[cur]=ori_char(ori);
        used++;
        if (used > L) { free(b); return 0; }
    }
    Sim ver=sim_make(); sim_compute(&ver,b);
    int ok = (ver.litCount==C);
    if (ok) memcpy(answer,b,(size_t)N);
    sim_free(&ver); free(b);
    return ok;
}

static int try_laser_path_both(const IntVec *path, char *answer) {
    if (laserCount != 1 || path->n < 2) return 0;
    int startCell = cell_of(lasers[0].r, lasers[0].c);
    for (int i=0;i<path->n;i++) if (path->a[i]==startCell) {
        if (try_laser_path_suffix(path, i, answer)) return 1;
        break;
    }
    IntVec rev; vec_init(&rev); vec_reserve(&rev, path->n);
    for (int i=path->n-1;i>=0;i--) vec_push(&rev, path->a[i]);
    for (int i=0;i<rev.n;i++) if (rev.a[i]==startCell) {
        if (try_laser_path_suffix(&rev, i, answer)) { vec_free(&rev); return 1; }
        break;
    }
    vec_free(&rev); return 0;
}

static void make_row_snake(IntVec *p, int topDown, int leftFirst) {
    vec_init(p); vec_reserve(p, N);
    for (int rr=0; rr<H; rr++) {
        int r = topDown ? rr : (H-1-rr);
        int ltr = (rr%2==0) ? leftFirst : !leftFirst;
        if (ltr) for (int c=0;c<W;c++) vec_push(p, cell_of(r,c));
        else for (int c=W-1;c>=0;c--) vec_push(p, cell_of(r,c));
    }
}
static void make_col_snake(IntVec *p, int leftToRightCols, int topFirst) {
    vec_init(p); vec_reserve(p, N);
    for (int cc=0; cc<W; cc++) {
        int c = leftToRightCols ? cc : (W-1-cc);
        int ttb = (cc%2==0) ? topFirst : !topFirst;
        if (ttb) for (int r=0;r<H;r++) vec_push(p, cell_of(r,c));
        else for (int r=H-1;r>=0;r--) vec_push(p, cell_of(r,c));
    }
}
static void make_even_row_cycle(IntVec *p, int flipRows, int flipCols) {
    vec_init(p);
    if (H<2 || W<2 || (H%2)!=0) return;
    vec_reserve(p, N);
    for (int c=0;c<W;c++) {
        int r=0, cc=c; if (flipRows) r=H-1-r; if (flipCols) cc=W-1-cc; vec_push(p, cell_of(r,cc));
    }
    for (int r0=1;r0<H;r0++) {
        if (r0%2==1) {
            for (int c0=W-1;c0>=1;c0--) { int r=r0,c=c0; if (flipRows) r=H-1-r; if (flipCols) c=W-1-c; vec_push(p, cell_of(r,c)); }
        } else {
            for (int c0=1;c0<W;c0++) { int r=r0,c=c0; if (flipRows) r=H-1-r; if (flipCols) c=W-1-c; vec_push(p, cell_of(r,c)); }
        }
    }
    for (int r0=H-1;r0>=1;r0--) { int r=r0,c=0; if (flipRows) r=H-1-r; if (flipCols) c=W-1-c; vec_push(p, cell_of(r,c)); }
}
static void make_even_col_cycle(IntVec *p, int flipRows, int flipCols) {
    vec_init(p);
    if (H<2 || W<2 || (W%2)!=0) return;
    vec_reserve(p, N);
    for (int r0=0;r0<H;r0++) { int r=r0,c=0; if (flipRows) r=H-1-r; if (flipCols) c=W-1-c; vec_push(p, cell_of(r,c)); }
    for (int c0=1;c0<W;c0++) {
        if (c0%2==1) {
            for (int r0=H-1;r0>=1;r0--) { int r=r0,c=c0; if (flipRows) r=H-1-r; if (flipCols) c=W-1-c; vec_push(p, cell_of(r,c)); }
        } else {
            for (int r0=1;r0<H;r0++) { int r=r0,c=c0; if (flipRows) r=H-1-r; if (flipCols) c=W-1-c; vec_push(p, cell_of(r,c)); }
        }
    }
    for (int c0=W-1;c0>=1;c0--) { int r=0,c=c0; if (flipRows) r=H-1-r; if (flipCols) c=W-1-c; vec_push(p, cell_of(r,c)); }
}

static int valid_row_move(int *rows, int k, int x, int y, int transition) {
    if (x==y) return 0;
    Laser las = lasers[0];
    if (k==0) {
        if (las.d==1 && y<=x) return 0;
        if (las.d==3 && y>=x) return 0;
    }
    int r=rows[k];
    if (!row_segment_no_wall(r,x,y)) return 0;
    if (!row_cats_covered_by_segment(r,x,y)) return 0;
    if (transition) {
        int nr=rows[k+1];
        if (orig[cell_of(r,y)] != '.') return 0;
        if (orig[cell_of(nr,y)] != '.') return 0;
    }
    return 1;
}
static int valid_col_move(int *cols, int k, int x, int y, int transition) {
    if (x==y) return 0;
    Laser las = lasers[0];
    if (k==0) {
        if (las.d==2 && y<=x) return 0;
        if (las.d==0 && y>=x) return 0;
    }
    int c=cols[k];
    if (!col_segment_no_wall(c,x,y)) return 0;
    if (!col_cats_covered_by_segment(c,x,y)) return 0;
    if (transition) {
        int nc=cols[k+1];
        if (orig[cell_of(y,c)] != '.') return 0;
        if (orig[cell_of(y,nc)] != '.') return 0;
    }
    return 1;
}

static int one_laser_adaptive_row_sweep(char *answer, int rowStep) {
    if (laserCount!=1 || C==0) return 0;
    Laser las=lasers[0];
    if (!(las.d==1 || las.d==3)) return 0;
    int *rows=(int*)malloc(sizeof(int)*(size_t)H);
    int M=0;
    for (int r=las.r; r>=0 && r<H; r+=rowStep) rows[M++]=r;
    if (M==0) { free(rows); return 0; }
    int BIG=1000000;
    int *cost=(int*)malloc(sizeof(int)*(size_t)M*W);
    int *parent=(int*)malloc(sizeof(int)*(size_t)M*W);
    if (!cost || !parent) exit(2);
    for (int i=0;i<M*W;i++) { cost[i]=BIG; parent[i]=-1; }
    cost[las.c]=0;
    for (int k=0;k+1<M;k++) {
        if ((k&7)==0 && timeExpired()) { free(rows); free(cost); free(parent); return 0; }
        for (int x=0;x<W;x++) if (cost[k*W+x] < BIG) {
            for (int y=0;y<W;y++) {
                if (!valid_row_move(rows,k,x,y,1)) continue;
                int nc=cost[k*W+x]+2;
                if (nc<cost[(k+1)*W+y] && nc<=L) { cost[(k+1)*W+y]=nc; parent[(k+1)*W+y]=x; }
            }
        }
    }
    int last=M-1, bestX=-1,bestY=-1,bestCost=BIG;
    for (int x=0;x<W;x++) if (cost[last*W+x] < BIG) {
        for (int y=0;y<W;y++) {
            if (!valid_row_move(rows,last,x,y,0)) continue;
            int nc=cost[last*W+x];
            if (nc<bestCost) { bestCost=nc; bestX=x; bestY=y; }
        }
    }
    if (bestX<0 || bestCost>L) { free(rows); free(cost); free(parent); return 0; }
    int *entry=(int*)malloc(sizeof(int)*(size_t)M);
    int *exitc=(int*)malloc(sizeof(int)*(size_t)M);
    entry[last]=bestX;
    for (int k=last;k>=1;k--) entry[k-1]=parent[k*W+entry[k]];
    for (int k=0;k+1<M;k++) exitc[k]=entry[k+1];
    exitc[last]=bestY;
    IntVec path; vec_init(&path); vec_reserve(&path, M*W+M+4);
    for (int k=0;k<M;k++) {
        int r=rows[k], x=entry[k], y=exitc[k];
        int step=(y>x)?1:-1;
        for (int c=x;;c+=step) {
            int ce=cell_of(r,c);
            if (path.n==0 || path.a[path.n-1]!=ce) vec_push(&path,ce);
            if (c==y) break;
        }
        if (k+1<M) {
            int ce=cell_of(rows[k+1],y);
            if (path.n==0 || path.a[path.n-1]!=ce) vec_push(&path,ce);
        }
    }
    int ok=try_laser_path_suffix(&path,0,answer);
    vec_free(&path); free(rows); free(cost); free(parent); free(entry); free(exitc);
    return ok;
}

static int one_laser_adaptive_col_sweep(char *answer, int colStep) {
    if (laserCount!=1 || C==0) return 0;
    Laser las=lasers[0];
    if (!(las.d==0 || las.d==2)) return 0;
    int *cols=(int*)malloc(sizeof(int)*(size_t)W);
    int M=0;
    for (int c=las.c; c>=0 && c<W; c+=colStep) cols[M++]=c;
    if (M==0) { free(cols); return 0; }
    int BIG=1000000;
    int *cost=(int*)malloc(sizeof(int)*(size_t)M*H);
    int *parent=(int*)malloc(sizeof(int)*(size_t)M*H);
    if (!cost || !parent) exit(2);
    for (int i=0;i<M*H;i++) { cost[i]=BIG; parent[i]=-1; }
    cost[las.r]=0;
    for (int k=0;k+1<M;k++) {
        if ((k&7)==0 && timeExpired()) { free(cols); free(cost); free(parent); return 0; }
        for (int x=0;x<H;x++) if (cost[k*H+x] < BIG) {
            for (int y=0;y<H;y++) {
                if (!valid_col_move(cols,k,x,y,1)) continue;
                int nc=cost[k*H+x]+2;
                if (nc<cost[(k+1)*H+y] && nc<=L) { cost[(k+1)*H+y]=nc; parent[(k+1)*H+y]=x; }
            }
        }
    }
    int last=M-1, bestX=-1,bestY=-1,bestCost=BIG;
    for (int x=0;x<H;x++) if (cost[last*H+x] < BIG) {
        for (int y=0;y<H;y++) {
            if (!valid_col_move(cols,last,x,y,0)) continue;
            int nc=cost[last*H+x];
            if (nc<bestCost) { bestCost=nc; bestX=x; bestY=y; }
        }
    }
    if (bestX<0 || bestCost>L) { free(cols); free(cost); free(parent); return 0; }
    int *entry=(int*)malloc(sizeof(int)*(size_t)M);
    int *exitr=(int*)malloc(sizeof(int)*(size_t)M);
    entry[last]=bestX;
    for (int k=last;k>=1;k--) entry[k-1]=parent[k*H+entry[k]];
    for (int k=0;k+1<M;k++) exitr[k]=entry[k+1];
    exitr[last]=bestY;
    IntVec path; vec_init(&path); vec_reserve(&path, M*H+M+4);
    for (int k=0;k<M;k++) {
        int c=cols[k], x=entry[k], y=exitr[k];
        int step=(y>x)?1:-1;
        for (int r=x;;r+=step) {
            int ce=cell_of(r,c);
            if (path.n==0 || path.a[path.n-1]!=ce) vec_push(&path,ce);
            if (r==y) break;
        }
        if (k+1<M) {
            int ce=cell_of(y, cols[k+1]);
            if (path.n==0 || path.a[path.n-1]!=ce) vec_push(&path,ce);
        }
    }
    int ok=try_laser_path_suffix(&path,0,answer);
    vec_free(&path); free(cols); free(cost); free(parent); free(entry); free(exitr);
    return ok;
}

static int one_laser_sweep_patterns(char *answer) {
    if (laserCount!=1 || C==0) return 0;
    if (!rowWall && !build_prefixes()) return 0;
    if (one_laser_adaptive_row_sweep(answer, 1)) return 1;
    if (one_laser_adaptive_row_sweep(answer, -1)) return 1;
    if (one_laser_adaptive_col_sweep(answer, 1)) return 1;
    if (one_laser_adaptive_col_sweep(answer, -1)) return 1;
    for (int a=0;a<2;a++) for (int b=0;b<2;b++) {
        IntVec p; make_row_snake(&p, a==0, b==0);
        int ok=try_laser_path_both(&p, answer); vec_free(&p); if (ok) return 1;
    }
    for (int a=0;a<2;a++) for (int b=0;b<2;b++) {
        IntVec p; make_col_snake(&p, a==0, b==0);
        int ok=try_laser_path_both(&p, answer); vec_free(&p); if (ok) return 1;
    }
    if (H%2==0) for (int fr=0;fr<2;fr++) for (int fc=0;fc<2;fc++) {
        IntVec p; make_even_row_cycle(&p,fr,fc);
        int ok=try_laser_path_both(&p,answer); vec_free(&p); if (ok) return 1;
    }
    if (W%2==0) for (int fr=0;fr<2;fr++) for (int fc=0;fc<2;fc++) {
        IntVec p; make_even_col_cycle(&p,fr,fc);
        int ok=try_laser_path_both(&p,answer); vec_free(&p); if (ok) return 1;
    }
    return 0;
}

static int trace_to_target_after_codes(const char *b, int startState, int targetCell, const unsigned char *forbidden, IntVec *states, int *endState) {
    ensure_seen_or_die();
    states->n=0;
    seenIter++;
    if (seenIter == 0) { memset(seenStamp,0,sizeof(unsigned short)*(size_t)N4); seenIter=1; }
    int st=startState;
    seenStamp[st]=seenIter;
    for (int steps=0; steps<=N4; steps++) {
        int ce=st>>2, d=st&3;
        int r=ce/W, c=ce%W;
        int nr=r+dr[d], nc=c+dc[d];
        if (!inside(nr,nc) || b[cell_of(nr,nc)]=='#') return 0;
        int ne=cell_of(nr,nc);
        char ch=b[ne];
        int nd=d;
        if (ch=='/') nd=refl[0][d];
        else if (ch=='\\') nd=refl[1][d];
        int ns=(ne<<2)|nd;
        if (forbidden && forbidden[ns] && ns!=startState) return 0;
        if (seenStamp[ns]==seenIter) return 0;
        seenStamp[ns]=seenIter;
        vec_push(states, ns);
        if (ne==targetCell) { *endState=ns; return 1; }
        st=ns;
    }
    return 0;
}

static void route_init(RouteRes *rr) { rr->ok=0; vec_init(&rr->codes); vec_init(&rr->states); rr->endState=-1; rr->cost=0; rr->newLit=0; }
static void route_free(RouteRes *rr) { vec_free(&rr->codes); vec_free(&rr->states); rr->ok=0; }
static void route_move(RouteRes *dst, RouteRes *src) { route_free(dst); *dst=*src; src->ok=0; vec_init(&src->codes); vec_init(&src->states); }

static int route_from_bfs(const char *b, int startState, int targetCat, int rem, const unsigned char *forbidden, const unsigned char *lit, const BFSData *bd, RouteRes *rr) {
    route_init(rr);
    if (targetCat < 0 || targetCat >= C) return 0;
    int tc=catCell[targetCat];
    int bestSt=-1,bestD=INF;
    for (int d=0; d<4; d++) {
        int st=(tc<<2)|d;
        if (bd->dist[st] < bestD) { bestD=bd->dist[st]; bestSt=st; }
    }
    if (bestSt<0 || bestD>rem) return 0;
    IntVec rev; vec_init(&rev);
    int st=bestSt;
    while (st!=-1 && st!=-2 && st!=startState) {
        int code=bd->pcode[st];
        if (code!=-1) vec_push(&rev, code);
        st=bd->parent[st];
    }
    if (st!=startState) { vec_free(&rev); return 0; }
    if (!clean_codes_from_rev(&rev, &rr->codes)) { vec_free(&rev); route_free(rr); return 0; }
    vec_free(&rev);
    if (rr->codes.n > rem) { route_free(rr); return 0; }
    char *nb=board_copy(b);
    if (!apply_codes_to_board(nb, &rr->codes)) { free(nb); route_free(rr); return 0; }
    vec_init(&rr->states); vec_reserve(&rr->states, N4>0?64:1);
    int endState=-1;
    if (!trace_to_target_after_codes(nb, startState, tc, forbidden, &rr->states, &endState)) { free(nb); route_free(rr); return 0; }
    int newLit=0;
    for (int i=0;i<rr->states.n;i++) {
        int id=catId[rr->states.a[i]>>2];
        if (id>=0 && !lit[id]) newLit++;
    }
    rr->ok=1; rr->endState=endState; rr->cost=rr->codes.n; rr->newLit=newLit;
    free(nb); return 1;
}

static int cmp_reach(const void *a, const void *b) {
    const Reach *x=(const Reach*)a, *y=(const Reach*)b;
    if (x->dist != y->dist) return x->dist - y->dist;
    if (x->pos != y->pos) return x->pos - y->pos;
    return x->id - y->id;
}

static int one_laser_route_greedy(char *answer, int mode) {
    if (laserCount != 1) return 0;
    char *b=board_copy(orig);
    unsigned char *lit=(unsigned char*)calloc((size_t)(C>0?C:1),1);
    unsigned char *forb=(unsigned char*)calloc((size_t)(N4>0?N4:1),1);
    Reach *reach=(Reach*)malloc(sizeof(Reach)*(size_t)(C>0?C:1));
    if (!lit || !forb || !reach) exit(2);
    int cur=(cell_of(lasers[0].r,lasers[0].c)<<2)|lasers[0].d;
    forb[cur]=1;
    int litCount=0, used=0;
    int id0=catId[cur>>2]; if (id0>=0) { lit[id0]=1; litCount=1; }
    int guard=0;
    while (litCount<C && used<L && !timeExpired() && guard++ <= C+L+5) {
        int rem=L-used;
        BFSData bd; bfs_alloc(&bd); bfs_from_state(b,cur,rem,forb,&bd);
        int rn=0;
        for (int id=0; id<C; id++) if (!lit[id]) {
            int tc=catCell[id], best=INF;
            for (int d=0; d<4; d++) if (bd.dist[(tc<<2)|d] < best) best=bd.dist[(tc<<2)|d];
            if (best<=rem) reach[rn++] = (Reach){best, tc, id};
        }
        if (rn==0) { bfs_free(&bd); break; }
        qsort(reach,rn,sizeof(Reach),cmp_reach);
        int candLim = rn < (N<=2500 ? 80 : 45) ? rn : (N<=2500 ? 80 : 45);
        long long bestScore=LLONG_MIN; int bestId=-1;
        RouteRes best; route_init(&best);
        for (int i=0;i<candLim;i++) {
            if ((i&7)==0 && timeExpired()) break;
            RouteRes rr;
            if (!route_from_bfs(b,cur,reach[i].id,rem,forb,lit,&bd,&rr)) continue;
            long long score;
            if (mode==0) score = -1000000LL*rr.cost + 10000LL*rr.newLit - reach[i].dist;
            else if (mode==1) score = 1000000LL*rr.newLit - 20000LL*rr.cost - reach[i].dist;
            else if (mode==2) score = 1000000LL*rr.newLit - 5000LL*rr.states.n - 10000LL*rr.cost;
            else score = -100000LL*rr.cost + 1000LL*rr.states.n + 50000LL*rr.newLit;
            if (score > bestScore) { bestScore=score; bestId=reach[i].id; route_move(&best,&rr); }
            route_free(&rr);
        }
        bfs_free(&bd);
        if (bestId<0) { route_free(&best); break; }
        if (!apply_codes_to_board(b,&best.codes)) { route_free(&best); break; }
        used += best.cost;
        for (int i=0;i<best.states.n;i++) {
            int ns=best.states.a[i]; forb[ns]=1;
            int id=catId[ns>>2]; if (id>=0 && !lit[id]) { lit[id]=1; litCount++; }
        }
        cur=best.endState; route_free(&best);
    }
    int ok=0;
    if (litCount==C) {
        Sim ver=sim_make(); sim_compute(&ver,b);
        ok=(ver.litCount==C); if (ok) memcpy(answer,b,(size_t)N); sim_free(&ver);
    }
    free(b); free(lit); free(forb); free(reach);
    return ok;
}

static int cmpMode;
static int cmp_cat_order(const void *pa, const void *pb) {
    int a=*(const int*)pa, b=*(const int*)pb;
    int ca=catCell[a], cb=catCell[b];
    int ra=ca/W, cola=ca%W, rb=cb/W, colb=cb%W;
    if (cmpMode==0 || cmpMode==1) {
        int va=(ra&1)?-cola:cola, vb=(rb&1)?-colb:colb;
        if (ra!=rb) return ra-rb; return va-vb;
    } else if (cmpMode==2 || cmpMode==3) {
        int va=(cola&1)?-ra:ra, vb=(colb&1)?-rb:rb;
        if (cola!=colb) return cola-colb; return va-vb;
    } else {
        int sr=lasers[0].r, sc=lasers[0].c;
        int da=abs(ra-sr)+abs(cola-sc), db=abs(rb-sr)+abs(colb-sc);
        if (da!=db) return (cmpMode==4) ? (da-db) : (db-da);
        if (ra!=rb) return ra-rb; return cola-colb;
    }
}

static void make_cat_order(int mode, int *order) {
    for (int i=0;i<C;i++) order[i]=i;
    cmpMode=mode;
    qsort(order,C,sizeof(int),cmp_cat_order);
    if (mode==1 || mode==3) reverse_ints(order,C);
}

static int one_laser_route_by_order(char *answer, const int *order) {
    if (laserCount != 1) return 0;
    char *b=board_copy(orig);
    unsigned char *lit=(unsigned char*)calloc((size_t)(C>0?C:1),1);
    unsigned char *forb=(unsigned char*)calloc((size_t)(N4>0?N4:1),1);
    if (!lit || !forb) exit(2);
    int cur=(cell_of(lasers[0].r,lasers[0].c)<<2)|lasers[0].d;
    forb[cur]=1;
    int litCount=0, used=0;
    int id0=catId[cur>>2]; if (id0>=0) { lit[id0]=1; litCount=1; }
    int guard=0;
    while (litCount<C && used<L && !timeExpired() && guard++ <= C+L+5) {
        int target=-1;
        for (int i=0;i<C;i++) if (!lit[order[i]]) { target=order[i]; break; }
        if (target<0) break;
        BFSData bd; bfs_alloc(&bd); bfs_from_state(b,cur,L-used,forb,&bd);
        RouteRes rr;
        int ok=route_from_bfs(b,cur,target,L-used,forb,lit,&bd,&rr);
        bfs_free(&bd);
        if (!ok || (rr.cost==0 && rr.newLit==0)) { if (ok) route_free(&rr); break; }
        if (!apply_codes_to_board(b,&rr.codes)) { route_free(&rr); break; }
        used += rr.cost;
        for (int i=0;i<rr.states.n;i++) {
            int ns=rr.states.a[i]; forb[ns]=1;
            int id=catId[ns>>2]; if (id>=0 && !lit[id]) { lit[id]=1; litCount++; }
        }
        cur=rr.endState; route_free(&rr);
    }
    int ok=0;
    if (litCount==C) {
        Sim ver=sim_make(); sim_compute(&ver,b);
        ok=(ver.litCount==C); if (ok) memcpy(answer,b,(size_t)N); sim_free(&ver);
    }
    free(b); free(lit); free(forb);
    return ok;
}

static int one_laser_route_suite(char *answer) {
    if (laserCount != 1 || C==0) return 0;
    for (int mode=0; mode<4 && !timeExpired(); mode++) if (one_laser_route_greedy(answer,mode)) return 1;
    int *order=(int*)malloc(sizeof(int)*(size_t)(C>0?C:1));
    if (!order) exit(2);
    for (int mode=0; mode<6 && !timeExpired(); mode++) {
        make_cat_order(mode,order);
        if (one_laser_route_by_order(answer,order)) { free(order); return 1; }
    }
    free(order); return 0;
}

static void pstate_free(PState *s) {
    free(s->codes); free(s->lit); free(s->forb);
    s->codes=NULL; s->lit=NULL; s->forb=NULL; s->codesN=0;
}
static PState pstate_clone_empty(const PState *src) {
    PState ns;
    ns.codes=(int*)malloc(sizeof(int)*(size_t)(L>0?L:1));
    ns.lit=(unsigned char*)malloc((size_t)(C>0?C:1));
    ns.forb=(unsigned char*)malloc((size_t)(N4>0?N4:1));
    if (!ns.codes || !ns.lit || !ns.forb) exit(2);
    memcpy(ns.codes, src->codes, sizeof(int)*(size_t)src->codesN);
    ns.codesN=src->codesN;
    memcpy(ns.lit, src->lit, (size_t)(C>0?C:1));
    memcpy(ns.forb, src->forb, (size_t)(N4>0?N4:1));
    ns.cur=src->cur; ns.litCount=src->litCount; ns.score=src->score;
    return ns;
}
static int merge_route_codes_into_state(PState *ns, const RouteRes *rr) {
    ensure_tmp_or_die();
    tmpStamp++;
    if (tmpStamp == 0) { memset(tmpCellStamp,0,sizeof(unsigned short)*(size_t)N); tmpStamp=1; }
    for (int i=0;i<ns->codesN;i++) {
        int ce=ns->codes[i]/2, ori=ns->codes[i]&1;
        tmpCellStamp[ce]=tmpStamp; tmpCellOri[ce]=ori;
    }
    for (int i=0;i<rr->codes.n;i++) {
        int code=rr->codes.a[i], ce=code/2, ori=code&1;
        if (tmpCellStamp[ce]==tmpStamp) {
            if (tmpCellOri[ce]!=ori) return 0;
        } else {
            if (ns->codesN >= L) return 0;
            tmpCellStamp[ce]=tmpStamp; tmpCellOri[ce]=ori;
            ns->codes[ns->codesN++]=code;
        }
    }
    return ns->codesN <= L;
}
static int cmp_pstate(const void *a, const void *b) {
    const PState *x=(const PState*)a, *y=(const PState*)b;
    if (x->score != y->score) return y->score - x->score;
    if (x->litCount != y->litCount) return y->litCount - x->litCount;
    return x->codesN - y->codesN;
}

static void pstate_array_free(PState *arr, int n) {
    for (int i=0;i<n;i++) pstate_free(&arr[i]);
    free(arr);
}

static int one_laser_prefix_beam_lite(char *answer) {
    if (laserCount != 1 || C==0) return 0;
    int K = (N<=2500 ? 220 : 120);
    int PER = (N<=2500 ? 26 : 14);
    long long perStateBytes = 4LL * (L > 0 ? L : 1) + (long long)(C > 0 ? C : 1) + (long long)(N4 > 0 ? N4 : 1) + 64LL;
    long long maxNextStates = 28000000LL / (perStateBytes > 1 ? perStateBytes : 1);
    if (maxNextStates < 12) return 0;
    while ((long long)K * PER > maxNextStates) {
        if (K > 16) K = (K * 3 + 3) / 4;
        else if (PER > 3) PER--;
        else break;
    }
    if ((long long)K * PER > maxNextStates) {
        PER = 2;
        K = (int)(maxNextStates / PER);
        if (K < 4) return 0;
    }
    PState *front=(PState*)malloc(sizeof(PState)*(size_t)K);
    PState init;
    init.codes=(int*)malloc(sizeof(int)*(size_t)(L>0?L:1)); init.codesN=0;
    init.lit=(unsigned char*)calloc((size_t)(C>0?C:1),1);
    init.forb=(unsigned char*)calloc((size_t)(N4>0?N4:1),1);
    if (!front || !init.codes || !init.lit || !init.forb) exit(2);
    init.cur=(cell_of(lasers[0].r,lasers[0].c)<<2)|lasers[0].d;
    init.forb[init.cur]=1;
    init.litCount=0; init.score=0;
    int id0=catId[init.cur>>2]; if (id0>=0) { init.lit[id0]=1; init.litCount=1; }
    front[0]=init;
    int frontN=1;
    Reach *reach=(Reach*)malloc(sizeof(Reach)*(size_t)(C>0?C:1));
    char *b=(char*)malloc((size_t)N);
    if (!reach || !b) exit(2);
    long long work=0;
    for (int round=0; round<=L && frontN>0 && !timeExpired(); round++) {
        PState *next=(PState*)malloc(sizeof(PState)*(size_t)(K*PER + PER + 4));
        int nextN=0, nextCap=K*PER + PER + 4;
        if (!next) exit(2);
        for (int fi=0; fi<frontN; fi++) {
            if (((++work)&7LL)==0 && timeExpired()) break;
            PState *st=&front[fi];
            board_from_codes(st->codes, st->codesN, b);
            Sim curSim=sim_make(); sim_compute(&curSim,b);
            if (curSim.litCount==C) { memcpy(answer,b,(size_t)N); sim_free(&curSim); pstate_array_free(front,frontN); pstate_array_free(next,nextN); free(reach); free(b); return 1; }
            sim_free(&curSim);
            if (st->codesN >= L) continue;
            int rem=L-st->codesN;
            BFSData bd; bfs_alloc(&bd); bfs_from_state(b,st->cur,rem,st->forb,&bd);
            int rn=0;
            for (int id=0; id<C; id++) if (!st->lit[id]) {
                int tc=catCell[id], best=INF;
                for (int d=0; d<4; d++) if (bd.dist[(tc<<2)|d] < best) best=bd.dist[(tc<<2)|d];
                if (best<=rem) reach[rn++]=(Reach){best,tc,id};
            }
            if (rn==0) { bfs_free(&bd); continue; }
            qsort(reach,rn,sizeof(Reach),cmp_reach);
            int lim=rn < PER*3 ? rn : PER*3;
            PState *local=(PState*)malloc(sizeof(PState)*(size_t)(lim>0?lim:1));
            int localN=0;
            if (!local) exit(2);
            for (int i=0;i<lim;i++) {
                if ((i&7)==0 && timeExpired()) break;
                RouteRes rr;
                if (!route_from_bfs(b,st->cur,reach[i].id,rem,st->forb,st->lit,&bd,&rr)) continue;
                if (rr.cost==0 && rr.newLit==0) { route_free(&rr); continue; }
                PState ns=pstate_clone_empty(st);
                if (!merge_route_codes_into_state(&ns,&rr)) { pstate_free(&ns); route_free(&rr); continue; }
                for (int j=0;j<rr.states.n;j++) {
                    int ps=rr.states.a[j]; ns.forb[ps]=1;
                    int cid=catId[ps>>2]; if (cid>=0 && !ns.lit[cid]) { ns.lit[cid]=1; ns.litCount++; }
                }
                ns.cur=rr.endState;
                board_from_codes(ns.codes, ns.codesN, b);
                Sim nsim=sim_make(); sim_compute(&nsim,b);
                if (nsim.litCount==C) { memcpy(answer,b,(size_t)N); sim_free(&nsim); route_free(&rr); pstate_free(&ns); bfs_free(&bd); pstate_array_free(local,localN); pstate_array_free(front,frontN); pstate_array_free(next,nextN); free(reach); free(b); return 1; }
                ns.score = nsim.litCount*100000 + ns.litCount*1000 + rr.newLit*500 - ns.codesN*200 - reach[i].dist*30;
                sim_free(&nsim); route_free(&rr);
                local[localN++] = ns;
            }
            bfs_free(&bd);
            qsort(local,localN,sizeof(PState),cmp_pstate);
            int keep=localN<PER?localN:PER;
            for (int i=0;i<keep;i++) {
                if (nextN < nextCap) next[nextN++] = local[i];
                else pstate_free(&local[i]);
            }
            for (int i=keep;i<localN;i++) pstate_free(&local[i]);
            free(local);
        }
        pstate_array_free(front,frontN);
        qsort(next,nextN,sizeof(PState),cmp_pstate);
        if (nextN > K) {
            for (int i=K;i<nextN;i++) pstate_free(&next[i]);
            nextN=K;
        }
        front=next; frontN=nextN;
    }
    pstate_array_free(front,frontN); free(reach); free(b);
    return 0;
}




typedef struct { int *codes; int n; int litScore; int entries; } BState;

static void bstate_free(BState *s) { free(s->codes); s->codes=NULL; s->n=0; }
static int cmp_bstate(const void *a, const void *b) {
    const BState *x=(const BState*)a, *y=(const BState*)b;
    if (x->litScore != y->litScore) return y->litScore - x->litScore;
    if (x->n != y->n) return x->n - y->n;
    return y->entries - x->entries;
}
static int merge_codes_plain(const int *base, int baseN, const IntVec *add, int *out, int *outN) {
    ensure_tmp_or_die();
    tmpStamp++;
    if (tmpStamp == 0) { memset(tmpCellStamp,0,sizeof(unsigned short)*(size_t)N); tmpStamp=1; }
    for (int i=0;i<baseN;i++) {
        int ce=base[i]/2, ori=base[i]&1;
        tmpCellStamp[ce]=tmpStamp; tmpCellOri[ce]=ori;
        out[i]=base[i];
    }
    int n=baseN;
    for (int i=0;i<add->n;i++) {
        int code=add->a[i], ce=code/2, ori=code&1;
        if (tmpCellStamp[ce] == tmpStamp) {
            if (tmpCellOri[ce] != ori) return 0;
        } else {
            if (n >= L) return 0;
            tmpCellStamp[ce]=tmpStamp; tmpCellOri[ce]=ori;
            out[n++]=code;
        }
    }
    *outN=n;
    return 1;
}

static int beam_constructive(char *answer) {
    double savedDeadline = globalDeadline;
    double localDeadline = now_sec() + (N <= 500 ? 2600.0/1000.0 : 1600.0/1000.0);
    if (localDeadline < globalDeadline) globalDeadline = localDeadline;
    int K = (N<=500 ? 500 : (N<=2500 ? 100 : 40));
    int PER = (N<=500 ? 60 : (N<=2500 ? 18 : 8));
    BState *front=(BState*)malloc(sizeof(BState)*(size_t)K);
    if (!front) exit(2);
    front[0].codes=(int*)malloc(sizeof(int)*(size_t)(L>0?L:1));
    if (!front[0].codes) exit(2);
    front[0].n=0; front[0].litScore=0; front[0].entries=0;
    int frontN=1;
    int *unlit=(int*)malloc(sizeof(int)*(size_t)(C>0?C:1));
    int *targets=(int*)malloc(sizeof(int)*(size_t)(C+2));
    char *b=(char*)malloc((size_t)N);
    if (!unlit || !targets || !b) exit(2);
    long long workCounter=0;
    for (int round=0; round<=L && frontN>0 && !timeExpired(); round++) {
        int nextCap = K*PER + PER + 4;
        BState *next=(BState*)malloc(sizeof(BState)*(size_t)nextCap);
        if (!next) exit(2);
        int nextN=0;
        for (int fi=0; fi<frontN; fi++) {
            if (((++workCounter)&31LL)==0 && timeExpired()) break;
            BState *st=&front[fi];
            board_from_codes(st->codes, st->n, b);
            Sim sim=sim_make(); sim_compute(&sim,b);
            if (sim.litCount==C) {
                memcpy(answer,b,(size_t)N);
                sim_free(&sim);
                for (int i=0;i<frontN;i++) bstate_free(&front[i]); free(front);
                for (int i=0;i<nextN;i++) bstate_free(&next[i]); free(next);
                free(unlit); free(targets); free(b); globalDeadline=savedDeadline; return 1;
            }
            if (st->n >= L) { sim_free(&sim); continue; }
            int rem=L-st->n;
            int un=0;
            for (int i=0;i<C;i++) if (!sim.lit[i]) unlit[un++]=i;
            int tn=0;
            long long wk=1LL*un*(N>0?N:1);
            if (wk <= 3000000LL) {
                for (int i=0;i<un;i++) targets[tn++]=unlit[i];
            } else {
                int take=un<80?un:80;
                for (int i=0;i<take;i++) targets[tn++]=unlit[(long long)i*un/take];
                if (un>0) targets[tn++]=unlit[un-1];
                qsort(targets,tn,sizeof(int),cmp_int_asc);
                int m=0; for (int i=0;i<tn;i++) if (i==0 || targets[i]!=targets[i-1]) targets[m++]=targets[i]; tn=m;
            }
            BState *local=(BState*)malloc(sizeof(BState)*(size_t)(tn>0?tn:1));
            if (!local) exit(2);
            int localN=0;
            for (int ti=0; ti<tn; ti++) {
                if (((++workCounter)&31LL)==0 && timeExpired()) break;
                IntVec add;
                if (!shortest_path_to_target(b,&sim,rem,targets[ti],&add)) { vec_free(&add); continue; }
                BState ns;
                ns.codes=(int*)malloc(sizeof(int)*(size_t)(L>0?L:1));
                if (!ns.codes) exit(2);
                if (!merge_codes_plain(st->codes, st->n, &add, ns.codes, &ns.n)) { free(ns.codes); vec_free(&add); continue; }
                vec_free(&add);
                char *nb=(char*)malloc((size_t)N);
                if (!nb) exit(2);
                board_from_codes(ns.codes,ns.n,nb);
                Sim nsim=sim_make(); sim_compute(&nsim,nb);
                if (nsim.litCount==C) {
                    memcpy(answer,nb,(size_t)N);
                    sim_free(&nsim); free(nb); free(ns.codes); sim_free(&sim);
                    for (int i=0;i<localN;i++) bstate_free(&local[i]); free(local);
                    for (int i=0;i<frontN;i++) bstate_free(&front[i]); free(front);
                    for (int i=0;i<nextN;i++) bstate_free(&next[i]); free(next);
                    free(unlit); free(targets); free(b); globalDeadline=savedDeadline; return 1;
                }
                ns.litScore=nsim.litCount*1000;
                ns.entries=nsim.entryCount;
                sim_free(&nsim); free(nb);
                local[localN++]=ns;
            }
            sim_free(&sim);
            qsort(local,localN,sizeof(BState),cmp_bstate);
            int keep=localN<PER?localN:PER;
            for (int i=0;i<keep;i++) {
                if (nextN<nextCap) next[nextN++]=local[i]; else bstate_free(&local[i]);
            }
            for (int i=keep;i<localN;i++) bstate_free(&local[i]);
            free(local);
        }
        for (int i=0;i<frontN;i++) bstate_free(&front[i]);
        free(front);
        qsort(next,nextN,sizeof(BState),cmp_bstate);
        if (nextN>K) { for (int i=K;i<nextN;i++) bstate_free(&next[i]); nextN=K; }
        front=next; frontN=nextN;
    }
    for (int i=0;i<frontN;i++) bstate_free(&front[i]); free(front);
    free(unlit); free(targets); free(b);
    globalDeadline=savedDeadline;
    return 0;
}



typedef struct { int cell, ori, need, scoreLit, scoreEntries; } Cand;
static char *exactBoard = NULL;
static long long exactNodes = 0, exactNodeLimit = 200000;
static double exactDeadline = 0.0;

static int cmp_cand_exact(const void *a, const void *b) {
    const Cand *x=(const Cand*)a, *y=(const Cand*)b;
    if (x->scoreLit != y->scoreLit) return y->scoreLit - x->scoreLit;
    if (x->need != y->need) return x->need - y->need;
    if (x->scoreEntries != y->scoreEntries) return y->scoreEntries - x->scoreEntries;
    if (x->cell != y->cell) return x->cell - y->cell;
    return x->ori - y->ori;
}

static int *reverse_dist_to_target(const char *b, int targetCat) {
    int *dist=(int*)malloc(sizeof(int)*(size_t)N4);
    if (!dist) exit(2);
    for (int i=0;i<N4;i++) dist[i]=INF;
    Deque dq; dq_init(&dq,N4+16);
    int tc=catCell[targetCat];
    for (int d=0; d<4; d++) { int st=(tc<<2)|d; dist[st]=0; dq_push_back(&dq,st); }
    long long pops=0;
    while (dq.size>0) {
        if (((++pops)&4095LL)==0 && (timeExpired() || now_sec()>exactDeadline)) break;
        int st=dq_pop_front(&dq);
        int ce=st>>2, dout=st&3;
        int r=ce/W, c=ce%W;
        int base=dist[st];
        char ch=b[ce];
        if (ch=='#') continue;
        for (int din=0; din<4; din++) {
            int pr=r-dr[din], pc=c-dc[din];
            if (!inside(pr,pc) || b[cell_of(pr,pc)]=='#') continue;
            int add=-1;
            if (ch=='/') { if (refl[0][din]==dout) add=0; }
            else if (ch=='\\') { if (refl[1][din]==dout) add=0; }
            else if (ch=='.') {
                if (din==dout) add=0;
                if (refl[0][din]==dout) add = (add<0 ? 1 : (add<1?add:1));
                if (refl[1][din]==dout) add = (add<0 ? 1 : (add<1?add:1));
            } else {
                if (din==dout) add=0;
            }
            if (add<0) continue;
            int pst=(cell_of(pr,pc)<<2)|din;
            if (base+add < dist[pst]) {
                dist[pst]=base+add;
                if (add==0) dq_push_front(&dq,pst); else dq_push_back(&dq,pst);
            }
        }
    }
    dq_free(&dq);
    return dist;
}

static int candidates_for_target_exact(const char *b, const Sim *sim, int targetCat, int rem, Cand **out) {
    *out=NULL;
    if (timeExpired() || now_sec()>exactDeadline) return 0;
    int *dist=reverse_dist_to_target(b,targetCat);
    unsigned char *used=(unsigned char*)calloc((size_t)N*2,1);
    Cand *res=(Cand*)malloc(sizeof(Cand)*(size_t)(N*2>1?N*2:1));
    if (!used || !res) exit(2);
    int rn=0;
    for (int i=0;i<sim->entryCount;i++) {
        int st=sim->entries[i], ce=st>>2, din=st&3;
        if (b[ce] != '.') continue;
        for (int ori=0; ori<2; ori++) {
            int dout=refl[ori][din];
            int pst=(ce<<2)|dout;
            if (dist[pst] >= INF) continue;
            int need=1+dist[pst];
            if (need<=rem) {
                int code=ce*2+ori;
                if (!used[code]) {
                    used[code]=1;
                    res[rn++] = (Cand){ce,ori,need,0,0};
                }
            }
        }
    }
    free(dist); free(used);
    *out=res;
    return rn;
}


typedef struct { int code, startState, need; } FirstCand2;
typedef struct { IntVec codes; int cost, litCount, entries; } RouteCand2;

static int cmp_firstcand2(const void *a, const void *b) {
    const FirstCand2 *x=(const FirstCand2*)a, *y=(const FirstCand2*)b;
    if (x->need != y->need) return x->need - y->need;
    if (x->code != y->code) return x->code - y->code;
    return x->startState - y->startState;
}

static int cmp_routecand2(const void *a, const void *b) {
    const RouteCand2 *x=(const RouteCand2*)a, *y=(const RouteCand2*)b;
    if (x->litCount != y->litCount) return y->litCount - x->litCount;
    if (x->cost != y->cost) return x->cost - y->cost;
    if (x->entries != y->entries) return y->entries - x->entries;
    return 0;
}

static void routecand2_free(RouteCand2 *r) { vec_free(&r->codes); r->cost=0; r->litCount=0; r->entries=0; }

static int route_codes_signature_equal(const IntVec *a, const IntVec *b) {
    if (a->n != b->n) return 0;
    for (int i=0;i<a->n;i++) {
        int found=0;
        for (int j=0;j<b->n;j++) if (a->a[i]==b->a[j]) { found=1; break; }
        if (!found) return 0;
    }
    return 1;
}

static int add_route_candidate(RouteCand2 **arr, int *n, int *cap, IntVec *codes, int litCount, int entries) {
    if (codes->n <= 0) return 0;
    for (int i=0;i<*n;i++) {
        if (route_codes_signature_equal(&(*arr)[i].codes, codes)) return 0;
    }
    if (*n >= *cap) {
        int nc = (*cap ? (*cap)*2 : 16);
        RouteCand2 *na = (RouteCand2*)realloc(*arr, sizeof(RouteCand2)*(size_t)nc);
        if (!na) exit(2);
        *arr=na; *cap=nc;
    }
    vec_copy(&(*arr)[*n].codes, codes);
    (*arr)[*n].cost = codes->n;
    (*arr)[*n].litCount = litCount;
    (*arr)[*n].entries = entries;
    (*n)++;
    return 1;
}

static int generate_route_candidates_to_target(const char *b, const Sim *sim, int targetCat, int rem, RouteCand2 **out, int *outN, int routeLimit) {
    *out=NULL; *outN=0;
    if (rem <= 0 || timeExpired() || now_sec()>exactDeadline) return 0;
    int *dist = reverse_dist_to_target(b, targetCat);
    FirstCand2 *first = (FirstCand2*)malloc(sizeof(FirstCand2)*(size_t)(sim->entryCount*2 + 2));
    unsigned char *used = (unsigned char*)calloc((size_t)(N*8>1?N*8:1), 1);
    if (!dist || !first || !used) exit(2);
    int fn=0;
    for (int i=0;i<sim->entryCount;i++) {
        int st=sim->entries[i], ce=st>>2, din=st&3;
        if (b[ce] != '.') continue;
        for (int ori=0; ori<2; ori++) {
            int code=ce*2+ori;
            int idx=code*4+din;
            if (used[idx]) continue;
            used[idx]=1;
            int start=(ce<<2)|refl[ori][din];
            int need=1;
            if (dist[start] >= INF) continue;
            need += dist[start];
            if (need <= rem) first[fn++] = (FirstCand2){code,start,need};
        }
    }
    free(dist); free(used);
    if (fn==0) { free(first); return 0; }
    qsort(first, fn, sizeof(FirstCand2), cmp_firstcand2);
    int firstLimit = routeLimit * 4;
    if (firstLimit < 120) firstLimit = 120;
    if (firstLimit > 420) firstLimit = 420;
    if (firstLimit > fn) firstLimit = fn;
    RouteCand2 *routes=NULL; int rn=0, rcap=0;
    for (int i=0; i<firstLimit; i++) {
        if ((i&7)==0 && (timeExpired() || now_sec()>exactDeadline)) break;
        int code=first[i].code, ce=code/2, ori=code&1;
        if (b[ce] != '.') continue;
        char *b2=board_copy(b);
        b2[ce]=ori_char(ori);
        BFSData bd; bfs_alloc(&bd);
        bfs_from_state(b2, first[i].startState, rem-1, NULL, &bd);
        RouteRes rr;
        int ok = route_from_bfs(b2, first[i].startState, targetCat, rem-1, NULL, sim->lit, &bd, &rr);
        bfs_free(&bd);
        if (ok) {
            IntVec all; vec_init(&all); vec_push(&all, code);
            if (append_new_codes(&all, &rr.codes) && all.n <= rem) {
                char *nb=board_copy(b);
                if (apply_codes_to_board(nb, &all)) {
                    Sim ns=sim_make(); sim_compute(&ns, nb);
                    if (ns.lit[targetCat] && ns.litCount > sim->litCount) {
                        add_route_candidate(&routes, &rn, &rcap, &all, ns.litCount, ns.entryCount);
                    }
                    sim_free(&ns);
                }
                free(nb);
            }
            vec_free(&all);
            route_free(&rr);
        }
        free(b2);
        if (rn >= routeLimit*3) {
            qsort(routes, rn, sizeof(RouteCand2), cmp_routecand2);
            for (int z=routeLimit; z<rn; z++) routecand2_free(&routes[z]);
            rn = routeLimit;
        }
    }
    free(first);
    if (rn==0) { free(routes); return 0; }
    qsort(routes, rn, sizeof(RouteCand2), cmp_routecand2);
    if (rn > routeLimit) {
        for (int i=routeLimit; i<rn; i++) routecand2_free(&routes[i]);
        rn = routeLimit;
    }
    *out=routes; *outN=rn;
    return rn;
}

static char *routeExactBoard = NULL;
static long long routeExactNodes = 0, routeExactNodeLimit = 70000;

static int choose_target_for_route_exact(const char *b, const Sim *sim, int rem) {
    int bestTarget=-1, bestCount=INF, bestMinNeed=INF, bestLitLine=-1;
    for (int id=0; id<C; id++) if (!sim->lit[id]) {
        if ((id&15)==0 && (timeExpired() || now_sec()>exactDeadline)) break;
        Cand *cands=NULL;
        int cn=candidates_for_target_exact(b, sim, id, rem, &cands);
        if (cn==0) { free(cands); return -2; }
        int mn=INF;
        for (int i=0;i<cn;i++) if (cands[i].need < mn) mn=cands[i].need;
        int lineCats=0;
        int tc=catCell[id], tr=tc/W, tc2=tc%W;
        for (int j=0;j<C;j++) if (!sim->lit[j]) {
            int cc=catCell[j];
            if (cc/W==tr || cc%W==tc2) lineCats++;
        }
        if (cn < bestCount ||
            (cn==bestCount && mn < bestMinNeed) ||
            (cn==bestCount && mn==bestMinNeed && lineCats > bestLitLine)) {
            bestTarget=id; bestCount=cn; bestMinNeed=mn; bestLitLine=lineCats;
        }
        free(cands);
    }
    return bestTarget;
}

static int place_codes_temp(IntVec *codes, int *changed, int *changedN) {
    *changedN=0;
    for (int i=0;i<codes->n;i++) {
        int code=codes->a[i], ce=code/2, ori=code&1;
        char want=ori_char(ori);
        if (routeExactBoard[ce] == want) continue;
        if (routeExactBoard[ce] != '.') {
            for (int j=0;j<*changedN;j++) routeExactBoard[changed[j]]='.';
            return 0;
        }
        routeExactBoard[ce]=want;
        changed[(*changedN)++]=ce;
    }
    return 1;
}

static void undo_codes_temp(int *changed, int changedN) {
    for (int i=0;i<changedN;i++) routeExactBoard[changed[i]]='.';
}

static int route_exact_dfs(int rem) {
    if (++routeExactNodes > routeExactNodeLimit) return 0;
    if (timeExpired() || now_sec()>exactDeadline) return 0;
    Sim sim=sim_make(); sim_compute(&sim, routeExactBoard);
    if (sim.litCount == C) { sim_free(&sim); return 1; }
    if (rem <= 0) { sim_free(&sim); return 0; }
    int target = choose_target_for_route_exact(routeExactBoard, &sim, rem);
    if (target < 0) { sim_free(&sim); return 0; }
    int routeLimit = 45;
    if (rem <= 3) routeLimit = 65;
    if (C > 120) routeLimit = 35;
    RouteCand2 *routes=NULL; int rn=0;
    generate_route_candidates_to_target(routeExactBoard, &sim, target, rem, &routes, &rn, routeLimit);
    if (rn==0) { sim_free(&sim); return 0; }
    int *changed=(int*)malloc(sizeof(int)*(size_t)(L>0?L:1));
    if (!changed) exit(2);
    for (int i=0;i<rn;i++) {
        if ((i&7)==0 && (timeExpired() || now_sec()>exactDeadline)) break;
        if (routes[i].cost > rem) continue;
        int changedN=0;
        if (!place_codes_temp(&routes[i].codes, changed, &changedN)) continue;
        if (route_exact_dfs(rem - changedN)) {
            free(changed);
            for (int z=0; z<rn; z++) routecand2_free(&routes[z]);
            free(routes); sim_free(&sim); return 1;
        }
        undo_codes_temp(changed, changedN);
    }
    free(changed);
    for (int i=0;i<rn;i++) routecand2_free(&routes[i]);
    free(routes); sim_free(&sim); return 0;
}

static int solve_route_exact_c(char *answer) {
    if (L > 10 || N > 400) return 0;
    double saved=globalDeadline;
    double loc=now_sec() + 2100.0/1000.0;
    exactDeadline = loc < saved ? loc : saved;
    routeExactBoard=board_copy(orig);
    routeExactNodes=0;
    routeExactNodeLimit = 45000;
    int ok=route_exact_dfs(L);
    if (ok) memcpy(answer, routeExactBoard, (size_t)N);
    free(routeExactBoard); routeExactBoard=NULL;
    globalDeadline=saved;
    return ok;
}

static int exact_dfs(int rem) {
    if (++exactNodes > exactNodeLimit) return 0;
    if (timeExpired() || now_sec()>exactDeadline) return 0;
    Sim sim=sim_make(); sim_compute(&sim,exactBoard);
    if (sim.litCount==C) { sim_free(&sim); return 1; }
    if (rem==0) { sim_free(&sim); return 0; }
    int *unlit=(int*)malloc(sizeof(int)*(size_t)(C>0?C:1));
    if (!unlit) exit(2);
    int un=0; for (int i=0;i<C;i++) if (!sim.lit[i]) unlit[un++]=i;
    int bestCount=INF, bestNeed=INF;
    Cand *bestCands=NULL; int bestN=0;
    int targetChecks=un;
    if (1LL*un*(N>0?N:1) > 2500000LL) targetChecks = un<60?un:60;
    for (int ti=0; ti<targetChecks; ti++) {
        int tid = (targetChecks==un) ? unlit[ti] : unlit[(long long)ti*un/targetChecks];
        Cand *cands=NULL; int cn=candidates_for_target_exact(exactBoard,&sim,tid,rem,&cands);
        if (cn==0) { free(cands); free(bestCands); free(unlit); sim_free(&sim); return 0; }
        int mn=INF; for (int i=0;i<cn;i++) if (cands[i].need<mn) mn=cands[i].need;
        if (cn<bestCount || (cn==bestCount && mn>bestNeed)) {
            free(bestCands); bestCands=cands; bestN=cn; bestCount=cn; bestNeed=mn;
            if (bestCount==1) break;
        } else free(cands);
    }
    free(unlit);
    if (!bestCands || bestN==0) { free(bestCands); sim_free(&sim); return 0; }
    for (int i=0;i<bestN;i++) {
        int ce=bestCands[i].cell;
        if (exactBoard[ce] != '.') { bestCands[i].scoreLit=-INF; continue; }
        exactBoard[ce]=ori_char(bestCands[i].ori);
        Sim s2=sim_make(); sim_compute(&s2,exactBoard);
        bestCands[i].scoreLit=s2.litCount;
        bestCands[i].scoreEntries=s2.entryCount;
        sim_free(&s2);
        exactBoard[ce]='.';
    }
    qsort(bestCands,bestN,sizeof(Cand),cmp_cand_exact);
    int limit=bestN;
    if (limit>120 && N>500) limit=120;
    for (int i=0;i<limit;i++) {
        if ((i&15)==0 && (timeExpired() || now_sec()>exactDeadline)) break;
        if (bestCands[i].scoreLit<0) continue;
        int ce=bestCands[i].cell;
        if (exactBoard[ce] != '.') continue;
        exactBoard[ce]=ori_char(bestCands[i].ori);
        if (exact_dfs(rem-1)) { free(bestCands); sim_free(&sim); return 1; }
        exactBoard[ce]='.';
    }
    free(bestCands); sim_free(&sim); return 0;
}

static int solve_exact_small(char *answer) {
    if (L>10) return 0;
    double saved=globalDeadline;
    double loc=now_sec() + (N<=500 ? 2400.0/1000.0 : 1200.0/1000.0);
    exactDeadline = loc < saved ? loc : saved;
    exactBoard=board_copy(orig);
    exactNodes=0;
    exactNodeLimit = (N<=500 ? 220000 : 60000);
    int ok=exact_dfs(L);
    if (ok) memcpy(answer,exactBoard,(size_t)N);
    free(exactBoard); exactBoard=NULL;
    globalDeadline=saved;
    return ok;
}


typedef struct { int code, cur, d, lit, entries, score; } PathAddCand;
typedef struct { int *codes; int n, cur, d, lit, entries, score; } PathStateC;

static void pathstate_free(PathStateC *s) { free(s->codes); s->codes=NULL; s->n=0; }
static int cmp_pathadd(const void *a, const void *b) {
    const PathAddCand *x=(const PathAddCand*)a, *y=(const PathAddCand*)b;
    if (x->score != y->score) return y->score - x->score;
    if (x->lit != y->lit) return y->lit - x->lit;
    if (x->entries != y->entries) return y->entries - x->entries;
    return x->code - y->code;
}
static int cmp_pathstate(const void *a, const void *b) {
    const PathStateC *x=(const PathStateC*)a, *y=(const PathStateC*)b;
    if (x->score != y->score) return y->score - x->score;
    if (x->lit != y->lit) return y->lit - x->lit;
    if (x->n != y->n) return x->n - y->n;
    return y->entries - x->entries;
}
static void sort_codes_small2(int *a, int n) {
    for (int i=1;i<n;i++) {
        int x=a[i], j=i-1;
        while (j>=0 && a[j]>x) { a[j+1]=a[j]; j--; }
        a[j+1]=x;
    }
}
static int exact_complete_from_board_c(const char *b, int rem, char *answer, double millis, long long nodes) {
    if (rem < 0 || timeExpired()) return 0;
    double saved=globalDeadline;
    double loc=now_sec() + millis/1000.0;
    exactDeadline = loc < saved ? loc : saved;
    exactBoard=board_copy(b);
    exactNodes=0;
    exactNodeLimit=nodes;
    int ok=exact_dfs(rem);
    if (ok) memcpy(answer, exactBoard, (size_t)N);
    free(exactBoard); exactBoard=NULL;
    globalDeadline=saved;
    return ok;
}

static int laser_path_beam_complete_c(char *answer) {
    if (N > 400 || L > 10 || C == 0) return 0;
    double savedDeadline=globalDeadline;
    double loc=now_sec() + 2400.0/1000.0;
    if (loc < globalDeadline) globalDeadline=loc;
    int K=2600, PER=34;
    
    int maxNext = K*PER + PER + 8;
    char *b=(char*)malloc((size_t)N);
    PathAddCand *local=(PathAddCand*)malloc(sizeof(PathAddCand)*(size_t)(N*2>2?N*2:2));
    unsigned short *localCodeStamp=(unsigned short*)calloc((size_t)(N*2>2?N*2:2), sizeof(unsigned short));
    unsigned char *scanSeen=(unsigned char*)malloc((size_t)(N4>1?N4:1));
    if (!b || !local || !localCodeStamp || !scanSeen) exit(2);
    unsigned short localIter=1;
    int completionAttempts=0;

    for (int li=0; li<laserCount && !timeExpired(); li++) {
        PathStateC *front=(PathStateC*)malloc(sizeof(PathStateC)*(size_t)K);
        if (!front) exit(2);
        front[0].codes=(int*)malloc(sizeof(int)*(size_t)(L>0?L:1));
        if (!front[0].codes) exit(2);
        front[0].n=0;
        front[0].cur=cell_of(lasers[li].r, lasers[li].c);
        front[0].d=lasers[li].d;
        front[0].lit=0; front[0].entries=0; front[0].score=0;
        int frontN=1;
        for (int depth=0; depth<=L && frontN>0 && !timeExpired(); depth++) {
            PathStateC *next=(PathStateC*)malloc(sizeof(PathStateC)*(size_t)maxNext);
            if (!next) exit(2);
            int nextN=0;
            for (int fi=0; fi<frontN && !timeExpired(); fi++) {
                PathStateC *st=&front[fi];
                board_from_codes(st->codes, st->n, b);
                Sim sim=sim_make(); sim_compute(&sim,b);
                if (sim.litCount==C) {
                    memcpy(answer,b,(size_t)N);
                    sim_free(&sim);
                    for (int i=0;i<frontN;i++) pathstate_free(&front[i]); free(front);
                    for (int i=0;i<nextN;i++) pathstate_free(&next[i]); free(next);
                    free(b); free(local); free(localCodeStamp); free(scanSeen);
                    globalDeadline=savedDeadline; return 1;
                }
                int missing=C-sim.litCount;
                int rem=L-st->n;
                if (rem>=0 && missing <= 4 && completionAttempts < 24) {
                    completionAttempts++;
                    if (exact_complete_from_board_c(b, rem, answer, 45.0, 10000LL)) {
                        sim_free(&sim);
                        for (int i=0;i<frontN;i++) pathstate_free(&front[i]); free(front);
                        for (int i=0;i<nextN;i++) pathstate_free(&next[i]); free(next);
                        free(b); free(local); free(localCodeStamp); free(scanSeen);
                        globalDeadline=savedDeadline; return 1;
                    }
                }
                if (st->n >= L) { sim_free(&sim); continue; }
                int cn=0;
                memset(scanSeen,0,(size_t)(N4>1?N4:1));
                localIter++;
                if (localIter==0) { memset(localCodeStamp,0,sizeof(unsigned short)*(size_t)(N*2>2?N*2:2)); localIter=1; }
                int r=st->cur/W, c=st->cur%W, d=st->d;
                r += dr[d]; c += dc[d];
                while (inside(r,c) && b[cell_of(r,c)] != '#') {
                    int ce=cell_of(r,c);
                    int ss=(ce<<2)|d;
                    if (scanSeen[ss]) break;
                    scanSeen[ss]=1;
                    if (b[ce]=='.') {
                        for (int ori=0; ori<2; ori++) {
                            int code=ce*2+ori;
                            if (localCodeStamp[code]==localIter) continue;
                            localCodeStamp[code]=localIter;
                            b[ce]=ori_char(ori);
                            Sim ev=sim_make(); sim_compute(&ev,b);
                            int delta=ev.litCount-sim.litCount;
                            int score=ev.litCount*100000 + delta*30000 + ev.entryCount*30 - (st->n+1)*500;
                            if (delta<=0) score += ev.entryCount*60;
                            local[cn++] = (PathAddCand){code, ce, refl[ori][d], ev.litCount, ev.entryCount, score};
                            if (ev.litCount==C) {
                                memcpy(answer,b,(size_t)N);
                                sim_free(&ev); b[ce]='.'; sim_free(&sim);
                                for (int i=0;i<frontN;i++) pathstate_free(&front[i]); free(front);
                                for (int i=0;i<nextN;i++) pathstate_free(&next[i]); free(next);
                                free(b); free(local); free(localCodeStamp); free(scanSeen);
                                globalDeadline=savedDeadline; return 1;
                            }
                            sim_free(&ev);
                            b[ce]='.';
                        }
                    }
                    char ch=b[ce];
                    if (ch=='/') d=refl[0][d];
                    else if (ch=='\\') d=refl[1][d];
                    r += dr[d]; c += dc[d];
                }
                sim_free(&sim);
                if (cn==0) continue;
                qsort(local, cn, sizeof(PathAddCand), cmp_pathadd);
                int keep=cn<PER?cn:PER;
                for (int ci=0; ci<keep && nextN<maxNext; ci++) {
                    PathStateC ns;
                    ns.codes=(int*)malloc(sizeof(int)*(size_t)(L>0?L:1));
                    if (!ns.codes) exit(2);
                    for (int j=0;j<st->n;j++) ns.codes[j]=st->codes[j];
                    ns.codes[st->n]=local[ci].code;
                    ns.n=st->n+1;
                    sort_codes_small2(ns.codes, ns.n);
                    ns.cur=local[ci].cur;
                    ns.d=local[ci].d;
                    ns.lit=local[ci].lit;
                    ns.entries=local[ci].entries;
                    ns.score=local[ci].score;
                    next[nextN++]=ns;
                }
            }
            for (int i=0;i<frontN;i++) pathstate_free(&front[i]);
            free(front);
            qsort(next,nextN,sizeof(PathStateC),cmp_pathstate);
            if (nextN>K) { for (int i=K;i<nextN;i++) pathstate_free(&next[i]); nextN=K; }
            front=next; frontN=nextN;
        }
        for (int i=0;i<frontN;i++) pathstate_free(&front[i]);
        free(front);
    }
    free(b); free(local); free(localCodeStamp); free(scanSeen);
    globalDeadline=savedDeadline;
    return 0;
}

static int simulate_lit_count_only(const char *b) {
    if (C == 0) return 0;
    ensure_seen_or_die();
    unsigned char *lit = (unsigned char*)calloc((size_t)C, 1);
    if (!lit) exit(2);
    int litCount = 0;
    seenIter++;
    if (seenIter == 0) { memset(seenStamp,0,sizeof(unsigned short)*(size_t)N4); seenIter=1; }
    for (int li=0; li<laserCount; li++) {
        int r=lasers[li].r, c=lasers[li].c, d=lasers[li].d;
        while (inside(r,c) && b[cell_of(r,c)] != '#') {
            int ce=cell_of(r,c);
            int st=(ce<<2)|d;
            if (seenStamp[st] == seenIter) break;
            seenStamp[st] = seenIter;
            int id=catId[ce];
            if (id >= 0 && !lit[id]) { lit[id]=1; litCount++; }
            char ch=b[ce];
            if (ch=='/') d=refl[0][d];
            else if (ch=='\\') d=refl[1][d];
            r += dr[d]; c += dc[d];
        }
    }
    free(lit);
    return litCount;
}



/* ---------------- D checkpoint helpers: large-board multi-laser routing ---------------- */

static int try_laser_path_suffix_for(int li, const IntVec *path, int idx, char *answer) {
    if (li < 0 || li >= laserCount) return 0;
    if (idx < 0 || idx+1 >= path->n) return 0;
    int firstDir = dir_between_cells(path->a[idx], path->a[idx+1]);
    if (firstDir != lasers[li].d) return 0;
    char *b = board_copy(orig);
    int used=0;
    for (int i=idx; i<path->n; i++) if (b[path->a[i]]=='#') { free(b); return 0; }
    for (int i=idx+1; i+1<path->n; i++) {
        int cur=path->a[i];
        int din=dir_between_cells(path->a[i-1], cur);
        int dout=dir_between_cells(cur, path->a[i+1]);
        if (din<0 || dout<0) { free(b); return 0; }
        if (din==dout) continue;
        int ori=-1;
        if (refl[0][din]==dout) ori=0;
        else if (refl[1][din]==dout) ori=1;
        else { free(b); return 0; }
        if (b[cur] != '.') { free(b); return 0; }
        b[cur]=ori_char(ori);
        used++;
        if (used > L) { free(b); return 0; }
    }
    Sim ver=sim_make(); sim_compute(&ver,b);
    int ok = (ver.litCount==C);
    if (ok) memcpy(answer,b,(size_t)N);
    sim_free(&ver); free(b);
    return ok;
}

static int try_laser_path_both_for(int li, const IntVec *path, char *answer) {
    if (li < 0 || li >= laserCount || path->n < 2) return 0;
    int startCell = cell_of(lasers[li].r, lasers[li].c);
    for (int i=0;i<path->n;i++) if (path->a[i]==startCell) {
        if (try_laser_path_suffix_for(li, path, i, answer)) return 1;
        break;
    }
    IntVec rev; vec_init(&rev); vec_reserve(&rev, path->n);
    for (int i=path->n-1;i>=0;i--) vec_push(&rev, path->a[i]);
    for (int i=0;i<rev.n;i++) if (rev.a[i]==startCell) {
        if (try_laser_path_suffix_for(li, &rev, i, answer)) { vec_free(&rev); return 1; }
        break;
    }
    vec_free(&rev); return 0;
}

static int try_laser_cycle_for(int li, const IntVec *cycle, char *answer) {
    if (li < 0 || li >= laserCount || cycle->n < 3) return 0;
    int startCell = cell_of(lasers[li].r, lasers[li].c);
    for (int pass=0; pass<2; pass++) {
        IntVec rot; vec_init(&rot); vec_reserve(&rot, cycle->n);
        int idx=-1;
        if (pass==0) {
            for (int i=0;i<cycle->n;i++) if (cycle->a[i]==startCell) { idx=i; break; }
            if (idx>=0) for (int k=0;k<cycle->n;k++) vec_push(&rot, cycle->a[(idx+k)%cycle->n]);
        } else {
            for (int i=cycle->n-1;i>=0;i--) if (cycle->a[i]==startCell) { idx=i; break; }
            if (idx>=0) for (int k=0;k<cycle->n;k++) { int j=idx-k; if (j<0) j+=cycle->n; vec_push(&rot, cycle->a[j]); }
        }
        if (idx>=0) {
            int ok=try_laser_path_suffix_for(li,&rot,0,answer);
            vec_free(&rot);
            if (ok) return 1;
        } else vec_free(&rot);
    }
    return 0;
}

static void make_row_strip_from_laser(IntVec *p, int li, int rowStep, int rowsCount) {
    vec_init(p);
    if (li < 0 || li >= laserCount || rowsCount <= 0) return;
    Laser las=lasers[li];
    if (!(las.d==1 || las.d==3)) return;
    vec_reserve(p, W*rowsCount + rowsCount + 4);
    int dir=las.d, r=las.r, startC=las.c;
    for (int k=0; k<rowsCount; k++) {
        if (r<0 || r>=H) break;
        int endC = (dir==1) ? (W-1) : 0;
        int step = (endC>=startC) ? 1 : -1;
        for (int c=startC;;c+=step) {
            int ce=cell_of(r,c);
            if (p->n==0 || p->a[p->n-1]!=ce) vec_push(p,ce);
            if (c==endC) break;
        }
        if (k+1 < rowsCount) {
            int nr=r+rowStep;
            if (nr<0 || nr>=H) break;
            int ce=cell_of(nr,endC);
            if (p->n==0 || p->a[p->n-1]!=ce) vec_push(p,ce);
            r=nr; startC=endC; dir ^= 2;
        }
    }
}

static void make_col_strip_from_laser(IntVec *p, int li, int colStep, int colsCount) {
    vec_init(p);
    if (li < 0 || li >= laserCount || colsCount <= 0) return;
    Laser las=lasers[li];
    if (!(las.d==0 || las.d==2)) return;
    vec_reserve(p, H*colsCount + colsCount + 4);
    int dir=las.d, c=las.c, startR=las.r;
    for (int k=0; k<colsCount; k++) {
        if (c<0 || c>=W) break;
        int endR = (dir==2) ? (H-1) : 0;
        int step = (endR>=startR) ? 1 : -1;
        for (int r=startR;;r+=step) {
            int ce=cell_of(r,c);
            if (p->n==0 || p->a[p->n-1]!=ce) vec_push(p,ce);
            if (r==endR) break;
        }
        if (k+1 < colsCount) {
            int nc=c+colStep;
            if (nc<0 || nc>=W) break;
            int ce=cell_of(endR,nc);
            if (p->n==0 || p->a[p->n-1]!=ce) vec_push(p,ce);
            c=nc; startR=endR; dir ^= 2;
        }
    }
}

static int multi_laser_strip_sweeps(char *answer) {
    if (C==0 || laserCount<=0) return 0;
    for (int li=0; li<laserCount && !timeExpired(); li++) {
        Laser las=lasers[li];
        if (las.d==1 || las.d==3) {
            for (int rs=-1; rs<=1; rs+=2) {
                int maxRows = rs>0 ? (H-las.r) : (las.r+1);
                for (int m=maxRows; m>=1 && !timeExpired(); m--) {
                    if (m < maxRows-8 && m != 1 && (m % 5) != 0) continue;
                    IntVec p; make_row_strip_from_laser(&p,li,rs,m);
                    int ok = (p.n>=2) && try_laser_path_suffix_for(li,&p,0,answer);
                    vec_free(&p); if (ok) return 1;
                }
            }
        }
        if (las.d==0 || las.d==2) {
            for (int cs=-1; cs<=1; cs+=2) {
                int maxCols = cs>0 ? (W-las.c) : (las.c+1);
                for (int m=maxCols; m>=1 && !timeExpired(); m--) {
                    if (m < maxCols-8 && m != 1 && (m % 5) != 0) continue;
                    IntVec p; make_col_strip_from_laser(&p,li,cs,m);
                    int ok = (p.n>=2) && try_laser_path_suffix_for(li,&p,0,answer);
                    vec_free(&p); if (ok) return 1;
                }
            }
        }
    }
    return 0;
}

static int multi_laser_simple_sweep_patterns(char *answer) {
    if (C==0 || laserCount<=0) return 0;
    for (int li=0; li<laserCount && !timeExpired(); li++) {
        for (int a=0;a<2 && !timeExpired();a++) for (int b=0;b<2;b++) {
            IntVec p; make_row_snake(&p, a==0, b==0);
            int ok=try_laser_path_both_for(li,&p,answer); vec_free(&p); if (ok) return 1;
        }
        for (int a=0;a<2 && !timeExpired();a++) for (int b=0;b<2;b++) {
            IntVec p; make_col_snake(&p, a==0, b==0);
            int ok=try_laser_path_both_for(li,&p,answer); vec_free(&p); if (ok) return 1;
        }
        if (H%2==0) for (int fr=0;fr<2 && !timeExpired();fr++) for (int fc=0;fc<2;fc++) {
            IntVec p; make_even_row_cycle(&p,fr,fc);
            int ok=try_laser_path_both_for(li,&p,answer); if (!ok) ok=try_laser_cycle_for(li,&p,answer); vec_free(&p); if (ok) return 1;
        }
        if (W%2==0) for (int fr=0;fr<2 && !timeExpired();fr++) for (int fc=0;fc<2;fc++) {
            IntVec p; make_even_col_cycle(&p,fr,fc);
            int ok=try_laser_path_both_for(li,&p,answer); if (!ok) ok=try_laser_cycle_for(li,&p,answer); vec_free(&p); if (ok) return 1;
        }
    }
    return 0;
}

typedef struct { int li; RouteRes rr; long long score; int valid; } MLBest;

static int multi_laser_route_greedy_mode(char *answer, int mode, double millis, int candLimit) {
    if (laserCount <= 0 || C == 0) return 0;
    double savedDeadline = globalDeadline;
    double localDeadline = now_sec() + millis/1000.0;
    if (localDeadline < globalDeadline) globalDeadline = localDeadline;

    char *b = board_copy(orig);
    unsigned char *lit = (unsigned char*)calloc((size_t)(C>0?C:1), 1);
    unsigned char *forb = (unsigned char*)calloc((size_t)(N4>0?N4:1), 1);
    Reach *reach = (Reach*)malloc(sizeof(Reach)*(size_t)(C>0?C:1));
    int *cur = (int*)malloc(sizeof(int)*(size_t)(laserCount>0?laserCount:1));
    if (!b || !lit || !forb || !reach || !cur) exit(2);

    for (int li=0; li<laserCount; li++) {
        cur[li] = (cell_of(lasers[li].r, lasers[li].c)<<2) | lasers[li].d;
        forb[cur[li]] = 1;
    }

    Sim sim = sim_make();
    int used = 0;
    int noProgress = 0;
    int guardLimit = C + L + 4*laserCount + 32;
    if (guardLimit < 64) guardLimit = 64;
    if (guardLimit > 20000) guardLimit = 20000;

    for (int guard=0; guard<guardLimit && used<=L && !timeExpired(); guard++) {
        sim_compute(&sim, b);
        if (sim.litCount == C) {
            memcpy(answer,b,(size_t)N);
            sim_free(&sim); free(b); free(lit); free(forb); free(reach); free(cur);
            globalDeadline=savedDeadline; return 1;
        }
        memcpy(lit, sim.lit, (size_t)(C>0?C:1));
        int rem = L - used;
        if (rem <= 0) break;

        MLBest best; best.valid=0; best.li=-1; best.score=LLONG_MIN; route_init(&best.rr);
        for (int li=0; li<laserCount && !timeExpired(); li++) {
            BFSData bd; bfs_alloc(&bd); bfs_from_state(b, cur[li], rem, forb, &bd);
            int rn=0;
            for (int id=0; id<C; id++) if (!lit[id]) {
                int tc=catCell[id], dist=INF;
                for (int d=0; d<4; d++) if (bd.dist[(tc<<2)|d] < dist) dist = bd.dist[(tc<<2)|d];
                if (dist <= rem) reach[rn++] = (Reach){dist, tc, id};
            }
            if (rn==0) { bfs_free(&bd); continue; }
            qsort(reach, rn, sizeof(Reach), cmp_reach);
            int lim = rn < candLimit ? rn : candLimit;
            for (int i=0; i<lim && !timeExpired(); i++) {
                RouteRes rr;
                if (!route_from_bfs(b, cur[li], reach[i].id, rem, forb, lit, &bd, &rr)) continue;
                if (rr.cost==0 && rr.newLit==0) { route_free(&rr); continue; }
                long long score;
                if (mode == 0) score = 1000000000LL*rr.newLit - 25000000LL*rr.cost + 6000LL*rr.states.n - reach[i].dist;
                else if (mode == 1) score = -1000000000LL*rr.cost + 15000000LL*rr.newLit + 5000LL*rr.states.n - reach[i].dist;
                else if (mode == 2) score = 1000000000LL*rr.newLit - 1000000LL*rr.states.n - 10000000LL*rr.cost;
                else score = 100000000LL*rr.newLit - 100000LL*reach[i].dist - 2000000LL*rr.cost + 1000LL*rr.states.n;
                if (!best.valid || score > best.score) {
                    if (best.valid) route_free(&best.rr);
                    best.valid=1; best.li=li; best.score=score; best.rr=rr;
                    rr.ok=0; vec_init(&rr.codes); vec_init(&rr.states);
                }
                route_free(&rr);
            }
            bfs_free(&bd);
        }
        if (!best.valid) { route_free(&best.rr); break; }
        int beforeLit = sim.litCount;
        if (!apply_codes_to_board(b, &best.rr.codes)) { route_free(&best.rr); break; }
        used += best.rr.cost;
        for (int i=0;i<best.rr.states.n;i++) forb[best.rr.states.a[i]] = 1;
        cur[best.li] = best.rr.endState;
        route_free(&best.rr);
        sim_compute(&sim, b);
        if (sim.litCount <= beforeLit) {
            noProgress++;
            if (noProgress >= 6) break;
        } else noProgress=0;
    }

    sim_compute(&sim, b);
    int ok = (sim.litCount == C);
    if (ok) memcpy(answer,b,(size_t)N);
    sim_free(&sim); free(b); free(lit); free(forb); free(reach); free(cur);
    globalDeadline=savedDeadline;
    return ok;
}

static int multi_laser_route_suite_d(char *answer) {
    if (C==0 || laserCount<=0) return 0;
    int cand1 = (N <= 2500 ? 36 : 18);
    int cand2 = (N <= 2500 ? 28 : 14);
    if (multi_laser_route_greedy_mode(answer, 0, N<=2500 ? 1400.0 : 1800.0, cand1)) return 1;
    if (multi_laser_route_greedy_mode(answer, 1, N<=2500 ? 1100.0 : 1400.0, cand2)) return 1;
    if (multi_laser_route_greedy_mode(answer, 2, N<=2500 ? 900.0 : 1100.0, cand2)) return 1;
    if (N <= 2500 && multi_laser_route_greedy_mode(answer, 3, 900.0, 24)) return 1;
    return 0;
}

static int nearest_constructive_d(char *answer, double millis) {
    if (C==0) return 0;
    double savedDeadline = globalDeadline;
    double localDeadline = now_sec() + millis/1000.0;
    if (localDeadline < globalDeadline) globalDeadline = localDeadline;
    char *b = board_copy(orig);
    IntVec codes; vec_init(&codes);
    Sim sim = sim_make();
    int stagnant=0;
    for (int iter=0; iter<=L && !timeExpired(); iter++) {
        sim_compute(&sim,b);
        if (sim.litCount==C) { memcpy(answer,b,(size_t)N); free(b); vec_free(&codes); sim_free(&sim); globalDeadline=savedDeadline; return 1; }
        if (codes.n >= L) break;
        IntVec add; vec_init(&add);
        if (!shortest_path_to_target(b,&sim,L-codes.n,-1,&add) || add.n==0) { vec_free(&add); break; }
        char *nb=board_copy(b);
        if (!apply_codes_to_board(nb,&add)) { free(nb); vec_free(&add); break; }
        Sim ns=sim_make(); sim_compute(&ns,nb);
        if (ns.litCount < sim.litCount) {
            stagnant++;
            if (stagnant>=4) { sim_free(&ns); free(nb); vec_free(&add); break; }
        } else if (ns.litCount == sim.litCount) {
            stagnant++;
            if (stagnant>=8) { sim_free(&ns); free(nb); vec_free(&add); break; }
        } else stagnant=0;
        if (!append_new_codes(&codes,&add)) { sim_free(&ns); free(nb); vec_free(&add); break; }
        free(b); b=nb; vec_free(&add); sim_free(&ns);
    }
    sim_compute(&sim,b);
    int ok=(sim.litCount==C);
    if (ok) memcpy(answer,b,(size_t)N);
    free(b); vec_free(&codes); sim_free(&sim); globalDeadline=savedDeadline; return ok;
}

static int solve(char *answer) {
    globalDeadline = now_sec() + 7.05;
    if (C == 0) { memcpy(answer, orig, (size_t)N); return 1; }
    if (simulate_lit_count_only(orig) == C) { memcpy(answer,orig,(size_t)N); return 1; }

    if (laserCount == 1) {
        if (one_laser_sweep_patterns(answer)) return 1;
        double saved=globalDeadline;
        double small=now_sec()+1.05; if (small < globalDeadline) globalDeadline=small;
        if (one_laser_prefix_beam_lite(answer)) { globalDeadline=saved; return 1; }
        if (one_laser_route_suite(answer)) { globalDeadline=saved; return 1; }
        globalDeadline=saved;
    } else if (N > 400) {
        if (multi_laser_strip_sweeps(answer)) return 1;
        if (multi_laser_simple_sweep_patterns(answer)) return 1;
    }

    if (N > 400 && !timeExpired()) {
        if (nearest_constructive_d(answer, 850.0)) return 1;
        if (multi_laser_route_suite_d(answer)) return 1;
    }

    /* Stable C baseline. */
    if (greedy_constructive(answer)) return 1;

    if (N <= 400 && L <= 10 && laserCount > 1 && !timeExpired()) {
        if (laser_path_beam_complete_c(answer)) return 1;
        if (solve_route_exact_c(answer)) return 1;
    }

    if (laserCount == 1 && !timeExpired()) {
        double saved=globalDeadline;
        double small=now_sec()+1.50; if (small < globalDeadline) globalDeadline=small;
        if (one_laser_prefix_beam_lite(answer)) { globalDeadline=saved; return 1; }
        if (one_laser_route_suite(answer)) { globalDeadline=saved; return 1; }
        globalDeadline=saved;
    }

    if (N > 400 && !timeExpired()) {
        if (multi_laser_route_suite_d(answer)) return 1;
    }
    if (beam_constructive(answer)) return 1;
    if (laserCount == 1 && one_laser_route_suite(answer)) return 1;
    if (solve_exact_small(answer)) return 1;
    return 0;
}

int main(void) {
    refl[0][0]=1; refl[0][1]=0; refl[0][2]=3; refl[0][3]=2;
    refl[1][0]=3; refl[1][3]=0; refl[1][2]=1; refl[1][1]=2;
    if (!read_input()) return 0;
    char *answer=(char*)malloc((size_t)N);
    if (!answer) exit(2);
    int ok=solve(answer);
    if (!ok) memcpy(answer,orig,(size_t)N);
    printf("%d %d %d\n", W,H,L_input);
    for (int r=0;r<H;r++) {
        fwrite(answer + r*W, 1, (size_t)W, stdout);
        putchar('\n');
    }
    return 0;
}