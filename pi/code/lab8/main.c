#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <ctype.h>

#define TEST 1

typedef struct {
    int day;
    int month;
    int year;
} Date;

/////////////////////////////////////////////////////////////////
// 8.1 funkcja bsearch2

#define FOOD_MAX  30   // max. liczba artyku³ów
#define ART_MAX   15   // max. d³ugoœæ nazwy artyku³u +1
#define RECORD_MAX 40  // max. d³ugoœæ rekordu (linii) w pliku

typedef struct {
    char art[ART_MAX];
    float price;
    float amount;
    Date valid_date;
} Food;

typedef int (*ComparFp)(const void *, const void *);

int date_cmp(const void *a, const void *b){
    Food *fir=(Food *)a;
    Food *sec=(Food *)b;
    int f=fir->valid_date.year-sec->valid_date.year;
    if (f!=0) return f;
    f=fir->valid_date.month-sec->valid_date.month;
    if (f!=0) return f;
    f=fir->valid_date.day-sec->valid_date.day;
    return f;
}

int food_cmp(const void *a, const void *b){
    Food *fir=(Food *)a;
    Food *sec=(Food *)b;
    int f=strcmp(fir->art,sec->art);
    if (f!=0) return f;
    double d=fir->price-sec->price;
    if (d>0) return 1;
    else if (d<0) return -1;
    return date_cmp(a,b);
}

void *bsearch2(const void *key, void *base, size_t nitems, size_t size, ComparFp compar, char *result) {
    Food *ptr=bsearch(key,base,nitems,size,compar);
    if (ptr!=NULL){
        *result=0;
        return ptr;
    }
    *result=0;
    int i=0;
    while (i<nitems && compar(((Food *)base)+i,key)<0){
        i++;
    }
    return ((Food *)base)+i;
}

void print_art(Food *p, size_t n, char *art) {
    for (int i=0; i<n; i++){
        if (strcmp(p[i].art,art)==0){
            printf("%.2f %.2f %d %d %d\n",p[i].price,p[i].amount,p[i].valid_date.day,p[i].valid_date.month,p[i].valid_date.year);
        }
    }
}

Food* add_record(Food *tab, size_t tab_size, int *np, ComparFp compar, Food *new) {
    char result;
    Food *ptr=(Food *)bsearch2(new,tab,*np,sizeof(Food),compar,&result);
    if (result){
        ptr->amount+=new->amount;
        return ptr;
    }
    *np=*np+1;
    if (*np==tab_size){
        return NULL;
    }
    for (Food *j=tab+*np;j>ptr;j--){
        *j=*(j-1);
    }
    *ptr=*new;
    return ptr;
}

int read_stream(Food *tab, size_t size, int no, FILE *stream) {
    int curr_size=0;
    Food new_f;
    for (int i=0; i<no; i++){
        fscanf(stream,"%s %f %f %d %d %d",new_f.art,&new_f.price,&new_f.amount,&new_f.valid_date.day,&new_f.valid_date.month,&new_f.valid_date.year);
        add_record(tab,size,&curr_size,food_cmp,&new_f);
    }
    return curr_size;
}

int read_stream0(Food *tab, size_t size, int no, FILE *stream) {
    int curr_size=0;
    Food new_f;
    for (int i=0; i<no; i++){
        fscanf(stream,"%s %f %f %d %d %d",new_f.art,&new_f.price,&new_f.amount,&new_f.valid_date.day,&new_f.valid_date.month,&new_f.valid_date.year);
        add_record(tab,size,&curr_size,food_cmp,&new_f);
    }
    return curr_size;
}

void create_date(Food *f, Date d, int a){
    char date[3][5];
    struct tm info;
    info.tm_year=d.year-1900;
    info.tm_mon=d.month-1;
    info.tm_mday=d.day+a;
    info.tm_hour=0;
    info.tm_min=0;
    info.tm_sec=0;
    info.tm_isdst=-1;
    int ret=mktime(&info);
    struct tm *data=localtime(&ret);
    strftime(date[0],5,"%d",data);
    strftime(date[1],5,"%m",data);
    strftime(date[2],5,"%Y",data);
    f->valid_date.day=atoi(date[0]);
    f->valid_date.month=atoi(date[1]);
    f->valid_date.year=atoi(date[2]);
}

float value(Food *food_tab, size_t n, Date curr_date, int anticip) {
    qsort(food_tab,n,sizeof(Food),date_cmp);
    Food example;
    create_date(&example,curr_date,anticip);
    Food *a=(Food *)bsearch(&example,food_tab,n,sizeof(Food),date_cmp);
    if (a==NULL) return 0.0f;
    float suma=a->amount*a->price;
    for(Food *j=a-1; j>=food_tab; j--){
        if (date_cmp(a,j)!=0) break;
        suma+=j->amount*j->price;
    }
    for(Food *j=a+1; j<food_tab+n; j++){
        if (date_cmp(a,j)!=0) break;
        suma+=j->amount*j->price;
    }
    return suma;
}

/////////////////////////////////////////////////////////////////
// 8.3 Sukcesja

#define CHILD_MAX  20   // maksymalna liczba przesuwanej w tablicy grupy osób (elementów tablicy)

enum Sex {F, M};
enum BOOL {no, yes};

struct Bit_data {
    enum Sex sex:1;
    enum BOOL pretendent:1;   // =no - nie pretenduje (panuje albo nie ¿yje) ale jest w zbiorze dla spójnoœci relacji.
};

typedef struct {
    char *name;
    struct Bit_data bits;
    Date born;
    char *parent;
} Person;

typedef struct {
       char *par_name;
       int index;
} Parent;    // strukturê mo¿na rozbudowaæ o pole liczby dzieci

const Date primo_date = { 28, 10, 2011 }; // Data zmiany kolejnoœci sukcesji

int person_cmp(const void *a, const void *b){
    Person *fir=(Person *)a;
    Person *sec=(Person *)b;
    if (fir->parent==NULL){
        if (sec->parent==NULL) return date_cmp(fir,sec);
        else return -1;
    }
    if (sec->parent==NULL) return 1;
    int c=strcmp(fir->parent,sec->parent);
    if (c!=0) return c;
    else return date_cmp(fir,sec);
}

int mystrcmp(const void *a, const void *b){
    Person *fir=(Person *)a;
    Person *sec=(Person *)b;
    if (fir->parent==NULL){
        if (sec->name==NULL) return 0;
        else return -1;
    }
    if (sec->name==NULL) return 1;
    return strcmp(fir->parent,sec->name);
}

int parent_cmp(const void *a, const void *b){
    Parent *fir=(Parent *)a;
    Parent *sec=(Parent *)b;
    return fir->index-sec->index;
}

int fill_indices_tab(Parent *idx_tab, Person *pers_tab, int size) {
    Parent *p;
    Person *r;
    Person new_p;
    for (p=idx_tab; p<idx_tab+size; p++){
        r=pers_tab;
        new_p.name=p->par_name;
        while (mystrcmp(r,&new_p)!=0){
            r++;
        }
        p->index=r-pers_tab;
    }
    qsort(idx_tab,size,sizeof(Parent),parent_cmp);
    return size;
}

void persons_shiftings(Person *person_tab, int size, Parent *idx_tab, int no_parents) {
    int to_move,to_copy;
    Person tab[size];
    for (Parent *p=idx_tab; p<idx_tab+no_parents-1; p++){
        to_move=(p+1)->index-p->index-1;
        to_copy=size-(p+1)->index;
        memcpy(tab,person_tab+p->index+1,to_move*sizeof(Person));
        memmove(person_tab+p->index+1,person_tab+(p+1)->index,to_copy*sizeof(Person));
        memcpy(person_tab+p->index+1+to_copy,tab,to_move*sizeof(Person));
        for (Parent *r=p+1; r<idx_tab+no_parents; r++){
            r->index-=to_move;
        }
    }
}

int cleaning(Person *person_tab,int n) {
    int to_remove=0;
    for (int i=0; i<n; i++){
        if (!person_tab[i].bits.pretendent){
            to_remove+=1;
            memmove(person_tab+i,person_tab+(i+1),(n-i-1)*sizeof(Person));
        }
    }
    return n-to_remove;
}

void print_person(const Person *p) {
    printf("%s\n", p->name);
}

void print_persons(const Person *person_tab, int n) {
    for(int i=1; i<=n; ++i, ++person_tab) printf("%2d %12s %s\n", i, person_tab->name, person_tab->parent);
    return;
}

int create_list(Person *person_tab, int n) {
    qsort(person_tab,n,sizeof(Person),person_cmp);
    Parent tab[n];
    int j=1;
    while (person_tab[j++].parent==NULL);
    char *last=person_tab[--j].parent;
    tab[0].par_name=NULL;
    tab[1].par_name=last;
    int s=1;
    for (int i=j; i<n; i++){
        if (!strcmp(person_tab[i].parent,last)) continue;
        ++s;
        tab[s].par_name=person_tab[i].parent;
        last=tab[s].par_name;
    }
    int size=fill_indices_tab(tab,person_tab,s);
    persons_shiftings(person_tab,n,tab,size);
    return cleaning(person_tab,n);
}

////////////////////////////////////////////////////////////////

int main(void)
{
    // Wszyscy potomkowie Jerzego VI (w kolejnoœci przypadkowej):
    Person person_tab[33]={
    {"Charles III",M,no,14,11,1948,"Elizabeth II"},
    {"Anne",F,yes,15,8,1950,"Elizabeth II"},
    {"Andrew",M,yes,19,2,1960,"Elizabeth II"},
    {"Edward",M,yes,10,3,1964,"Elizabeth II"},
    {"David",M,yes,3,11,1961,"Margaret"},
    {"Sarah",F,yes,1,5,1964,"Margaret"},
    {"William",M,yes,21,6,1982,"Charles III"},
    {"Henry",M,yes,15,9,1984,"Charles III"},
    {"Peter",M,yes,15,11,1977,"Anne"},
    {"Zara",F,yes,15,5,1981,"Anne"},
    {"Beatrice",F,yes,8,8,1988,"Andrew"},
    {"Eugenie",F,yes,23,3,1990,"Andrew"},
    {"James",M,yes,17,12,2007,"Edward"},
    {"Louise",F,yes,8,11,2003,"Edward"},
    {"Charles",M,yes,1,7,1999,"David"},
    {"Margarita",F,yes,14,5,2002,"David"},
    {"Samuel",M,yes,28,7,1996,"Sarah"},
    {"Arthur",M,yes,6,5,2019,"Sarah"},
    {"Georg",M,yes,22,7,2013,"William"},
    {"George VI",M,no,14,12,1895,NULL},
    {"Charlotte",F,yes,22,5,2015,"William"},
    {"Louis",M,yes,23,4,2018,"William"},
    {"Archie",M,yes,6,5,2019,"Henry"},
    {"Lilibet",F,yes,4,6,2021,"Henry"},
    {"Savannah",F,yes,29,12,2010,"Peter"},
    {"Isla",F,yes,29,3,2012,"Peter"},
    {"Mia",F,yes,17,1,2014,"Zara"},
    {"Lena",F,yes,18,6,2018,"Zara"},
    {"Elizabeth II",F,no,21,4,1925,"George VI"},
    {"Margaret",F,no,21,8,1930,"George VI"},
    {"Lucas",M,yes,21,3,2021,"Zara"},
    {"Sienna",F,yes,18,9,2021,"Beatrice"},
    {"August",M,yes,9,2,2021,"Eugenie"}
    };

    int to_do, no;
    size_t size, n;
        Food food_tab[FOOD_MAX];
        char buff[ART_MAX];
        FILE *file;
    if(TEST) printf("Wpisz nr zadania (1 - 3) ");
    scanf("%d", &to_do);

    switch (to_do) {
    case 1:  // bsearch2
        if (TEST) printf("Wpisz liczbe linii danych: ");
        scanf("%d",&no); getchar();
        if(TEST) file = stdin;
        else {
            scanf("%s",buff);
            file = fopen(buff,"r");
//            file = fopen("foods0.txt","r");
            if(file==NULL) { printf("Error 1\n"); break; }
        }
        if (TEST) printf("W %d liniach wpisuj dane: nazwa cena ilosc dd mm yyyy: \n",no);
        n = read_stream(food_tab,FOOD_MAX,no,file);
        if (TEST) printf("Wpisz nazwe artykulu: ");
        scanf("%s",buff);
        print_art(food_tab,n,buff);
        break;
    case 2: // proste sortowanie struktur
        if (TEST) printf("Wpisz liczbe linii danych: ");
        scanf("%d",&no); getchar();
        if(TEST) file = stdin;
        else {
            scanf("%s",buff);
            file = fopen(buff,"r");
//            file = fopen("foods0.txt","r");
            if(file==NULL) { printf("Error 1\n"); break; }
        }
        if (TEST) printf("W %d liniach wpisuj dane: nazwa cena ilosc dd mm yyyy: \n",no);
        n = read_stream0(food_tab,FOOD_MAX,no,file);
        Date curr_date;
        int anticip;
        if (TEST) printf("Wpisz date odniesienia dd mm yyyy: ");
        scanf("%d %d %d",&curr_date.day,&curr_date.month,&curr_date.year);
        if (TEST) printf("Wpisz roznice dni: ");
        scanf("%d",&anticip);
        printf("%.2f\n",value(food_tab,n,curr_date,anticip));
        break;
    case 3: // sukcesja
        if(TEST==1) printf("Wpisz pozycjê w kolejce (liczona od 1): ");
        scanf("%d",&no);
        int no_persons = sizeof(person_tab) / sizeof(Person);
        no_persons = create_list(person_tab,no_persons);
        if(TEST) print_persons(person_tab,no_persons);
        else print_person(person_tab+no-1);
        break;
    default:
        printf("NOTHING TO DO FOR %d\n", to_do);
    }
    return 0;
}