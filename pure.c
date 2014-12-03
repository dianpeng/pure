#include "pure.h"
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <errno.h>
#include <stdint.h>
#include <math.h>
#include <stdio.h>
#include <stdarg.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#ifdef _WIN32
#define STRDUP _strdup
#else
#define STRDUP strdup
#endif /* _WIN32 */

#define PURE_MAX_VALUE_SLAB 256
#define PURE_MAX_CALLBACK 128
#define PURE_MAX_CTX_VAR 64
#define PURE_MAX_USER_FUNC 16
#define PURE_MAX_FUNC_PAR 32
#define PURE_MAX_LOCAL_VAR 16
#define PURE_MAX_ARRAY_LEN 16
#define PURE_MAX_LOCAL_BUF_SIZE 32
#define PURE_MAX_VARNAME 32 /* You should *make sure* that MAX_VARNAME <= MAX_LOCAL_BUF_SIZE */
#define PURE_MAX_CALL_STACK 200 /* The max recursive function you may call */
#define PURE_MAX_USER_MAP_SIZE 12

enum {
    PURE_EC_NO_ERROR,
    PURE_EC_NO_SUCH_FUNC,
    PURE_EC_NO_SUCH_VALUE,
    PURE_EC_FUNCTION_EXECUTION_ERROR,
    PURE_EC_UNKNOWN_CONSTANT_SYMBOL,
    PURE_EC_UNKNOWN_UNARY_OPRAND,
    PURE_EC_ONLY_NUMBER_CAN_BE_FACTOR,
    PURE_EC_ONLY_NUMBER_CAN_BE_TERM,
    PURE_EC_TYPE_ERROR_FOR_COMPARTOR,
    PURE_EC_DIV_ZERO,
    PURE_EC_ARRAY_EXPECT_RIGHT_SQURA,
    PURE_EC_UNKNOWN_NUMBER,
    PURE_EC_UNKNOWN_STRING_LITERAL,
    PURE_EC_UNKNOWN_VARIABLE,
    PURE_EC_INVALID_FOR_LOOP,
    PURE_EC_BREAK_IN_NONE_LOOP_SCOPE,
    PURE_EC_FOR_LOOP_EXPECT_VAR,
    PURE_EC_SYNTAX_ERROR,
    PURE_EC_FUNCTION_NO_BODY,
    PURE_EC_FUNCTION_REDEFINE,
    PURE_EC_TOO_MANY_FUNCTION_PARAMETERS,
    PURE_EC_EXPECT_SEMICON,
    PURE_EC_UNKNOWN_LEFT_HANDSIDE_VALUE,
    PURE_EC_INDEX_ON_NON_ARRAY_OR_MAP,
    PURE_EC_ARRAY_INDEX_SHOULD_BE_INTEGER,
    PURE_EC_MAP_INDEX_SHOULD_BE_STRING,
    PURE_EC_NO_SUCH_FILE,
    PURE_EC_STACK_OVERFLOW,
    PURE_EC_UNKNOWN_MAP_KEY,
    PURE_EC_EXPECT_COLON,
    PURE_EC_EXPECT_COMMA,
    PURE_EC_GLOBAL_SCOPE_SYNTAX_ERROR
};

/* A simple open addressing hash table implementation. Only insertion/query support,
 * deletion is not needed for us . */

struct symbol_entry {
    uint32_t hash;
    char*key;
    char buf[PURE_MAX_VARNAME];
    void* mem;
    struct symbol_entry* next;
};

struct symbol_table {
    size_t mem_sz;
    size_t cap;
    size_t sz;
    void* entry;
};

#define symbol_table_init(x,sz) memset((x),0,sizeof(struct symbol_table)*(sz))

#define cast(t,v) ((t)(v))

#define symbol_table_entry(t,idx) cast(struct symbol_entry*,cast(char*,(t)->entry)+(idx)*(sizeof(struct symbol_entry)+(t)->mem_sz))

static
void symbol_table_delete( struct symbol_table* tb );

uint32_t symbol_table_hash ( const char * data, size_t len ) {
    int val = 0;
    size_t i ;
    for( i = 0 ; i < len ; ++i ) {
        val = val ^ ((val<<5)+(val>>2)+(int)data[i]);
    }
    return val;
}

static
struct symbol_entry* symbol_table_insert_slot( struct symbol_table* tb , const char* key , size_t len , uint32_t hs , int* insert ) {
    size_t idx;
    struct symbol_entry* ent;
    assert( tb->cap > tb->sz );

    idx = hs & (tb->cap-1);
    ent = symbol_table_entry(tb,idx);

    if( ent->mem == NULL ) {
        *insert = 1;
        goto done;
    } else {
        /* Checking if this slot has been used before or collision happened */
        struct symbol_entry* tmp = ent;
        for( ; ent != NULL && ent->hash != hs && strcmp(ent->key,key) != 0 ; 
            tmp = ent , ent = ent->next ) ;
            if( ent != NULL ) {
                /* Existed, just return this one */
                assert( ent->hash == hs );
                assert( strcmp(ent->key,key) == 0 );
                *insert = 0;
                return ent;
            } else {
                uint32_t h = hs;
                for( ; symbol_table_entry(tb,(++h & (tb->cap-1)))->mem == NULL ; );
                ent = symbol_table_entry(tb,(++h & (tb->cap-1)));
                tmp->next = ent;
                *insert = 1;
                goto done;
            }
    }
done:
    ++tb->sz;
    ent->hash = hs;
    if( len >= PURE_MAX_VARNAME ) {
        ent->key = STRDUP(key);
    } else {
        strcpy(ent->key,key);
    }
    ent->next = NULL;
    ent->mem = cast(char*,ent)+sizeof(struct symbol_entry);
    return ent;
}

static
void symbol_table_rehash( struct symbol_table* tb ) {
    size_t ncap = tb->cap * 2;
    struct symbol_table tmp;
    size_t i;

    tmp.entry = calloc(ncap,sizeof(struct symbol_entry)+tb->mem_sz);
    tmp.cap = ncap;
    tmp.sz = 0;
    tmp.mem_sz = tb->mem_sz;

    for( i = 0 ; i < tmp.cap ; ++i ) {
        struct symbol_entry* e = symbol_table_entry(&tmp,i);
        e->key = e->buf;
    }

    for( i = 0 ; i < tb->sz ; ++i ) {
        int insert;
        struct symbol_entry* e = symbol_table_entry(tb,i);
        struct symbol_entry*
            ent = symbol_table_insert_slot(&tmp,e->key,strlen(e->key),e->hash,&insert);
        assert(insert == 1);
        assert(ent != NULL);
        memcpy(ent->mem,e->mem,tb->mem_sz);
    }
    /* delete original one */
    tmp.sz = tb->sz;
    symbol_table_delete(tb);
    *tb = tmp;
}

static
void symbol_table_create( struct symbol_table* tb , size_t cap , size_t mem_sz ) {
    if(cap >1)
        cap /= 2;
    tb->cap = cap;
    tb->sz = 0;
    tb->mem_sz = mem_sz;
    tb->entry = NULL;
    symbol_table_rehash(tb);
}

static
void symbol_table_resume( struct symbol_table* tb , size_t cap , size_t mem_sz ) {
    if( tb->cap == 0 ) {
        symbol_table_create(tb,cap,mem_sz);
    }
}

static
void* symbol_table_insert( struct symbol_table* tb , const char* key , int* insert ) {
    size_t len = strlen(key);
    struct symbol_entry* e ;
    uint32_t hs = symbol_table_hash(key,len);
    if( len >= PURE_MAX_VARNAME ) {
        *insert = 0;
        return NULL;
    } 
    if( tb->cap == tb->sz )
        symbol_table_rehash(tb);
    e = symbol_table_insert_slot(tb,key,len,hs,insert);
    return e->mem;
}

static
void* symbol_table_query( struct symbol_table* tb , const char* key ) {
    struct symbol_entry* ent;
    size_t len = strlen(key);
    uint32_t hs = symbol_table_hash(key,len);
    size_t idx = hs &(tb->cap-1);
    ent = symbol_table_entry(tb,idx);
    if( ent->mem == NULL )
        return NULL;
    else {
        for( ; ent != NULL && ent->hash != hs && strcmp(ent->key,key) != 0 ; ent = ent->next );
        return ent == NULL ? NULL : ent->mem;
    }
}

static
void symbol_table_delete( struct symbol_table* tb ) {
    size_t i;
    if( tb->entry != NULL ) {
        for( i = 0 ; i < tb->cap ; ++i ) {
            struct symbol_entry* e = symbol_table_entry(tb,i);
            if( e->key != e->buf ) {
                free(e->key);
            }
        }
        free(tb->entry);
    }
    tb->mem_sz = tb->cap = tb->sz = 0;
}

static
void symbol_table_clean( struct symbol_table* tb ) {
    size_t i;
    if( tb->entry != NULL ) {
        /* reset the pointer to point to the correct position */
        for( i = 0 ; i < tb->cap ; ++i ) {
            symbol_table_entry(tb,i)->mem = NULL;
        }
    }
    tb->sz = 0;
}

static
int symbol_table_iter_deref( struct symbol_table* tb , int idx, void** data , const char** name );

static
int symbol_table_iter_start( struct symbol_table* tb , void** data , const char** name ) {
    if( tb->sz == 0 )
        return tb->cap+1;
    return symbol_table_iter_deref(tb,0,data,name);
}

static
int symbol_table_iter_has_next( struct symbol_table* tb , int idx ) {
    return idx <= cast(int,tb->cap);
}

static
int symbol_table_iter_deref( struct symbol_table* tb , int idx, void** data , const char** name ) {
    assert( symbol_table_iter_has_next(tb,idx) );
    for( ; symbol_table_entry(tb,idx)->mem == NULL && cast(size_t,idx) < (tb->cap) ; ++idx );
    *data = symbol_table_entry(tb,idx)->mem;
    *name = symbol_table_entry(tb,idx)->key;
    return idx == (tb->cap)-1 ? idx = (tb->cap)+1 : idx+1;
}

/* =============================
 * A simple memory pool
 * ============================*/

struct slab {
    size_t obj_sz;
    size_t page_sz;
    void* cur;
    void* page_list;
};

struct page {
    struct page* next;
};

struct header {
    struct header* next;
};

/* The size is in bytes */
static
void grow( struct slab* sl ) {
    size_t psz = sl->page_sz * 2;
    void* new_pool = malloc( psz * sl->obj_sz + sizeof(void*) );
    struct header* h ;
    struct header* ptr;
    size_t i = 0;

    /* link into the slab page queue */
    cast(struct page*,new_pool)->next = 
        cast(struct page*,sl->page_list);
    sl->page_list = new_pool;
    new_pool=cast(char*,new_pool)+sizeof(void*);

    ptr = cast(struct header*,new_pool);
    h=ptr;
    for( ; i < psz-1 ; ++i ) {
        ptr->next = cast(struct header*,cast(char*,ptr) + sl->obj_sz);
        ptr = cast(struct header*,cast(char*,ptr)+sl->obj_sz);
    }
    ptr->next = cast(struct header*,sl->cur);
    sl->cur = h;
    sl->page_sz = psz;
}

#define ALIGN(x,a) (((x) + (a) - 1) & ~((a) - 1))

static
void slab_create( struct slab* slb , size_t sz , size_t page_sz ) {
    assert(page_sz != 0);
    assert(sz != 0);
    slb->obj_sz = ALIGN(sz,sizeof(void*));
    slb->page_sz = page_sz;
    slb->cur = slb->page_list = NULL;
    grow(slb);
}

static
void slab_destroy( struct slab* slb ) {
    while( slb->page_list != NULL ) {
        void* n = cast(struct page*,slb->page_list)->next;
        free(slb->page_list);
        slb->page_list = n;
    }
    slb->cur = slb->page_list = NULL;
    slb->obj_sz = slb->page_sz = 0;
}

static
void* slab_malloc( struct slab* slb ) {
    void* next;
    if( slb->cur == NULL )
        grow(slb);
    assert(slb->cur != NULL);
    next = slb->cur;
    slb->cur = cast(struct header*,next)->next;
    return next;
}

static
void slab_free( struct slab* slb , void* ptr ) {
    cast(struct header*,ptr)->next = cast(struct header*,slb->cur);
    slb->cur = ptr;
}

/* ====================================
 * pure value .
 * ===================================*/
struct pure_array {
    struct pure_value* arr; /* array of this */
    size_t cap; /* capacity of this pure array */
    size_t sz;  /* current used pointer of the array */
};

struct pure_shared_value {
    size_t ref_cnt;
    union {
        struct {
            const char* c_str;
            size_t sz;
        } str;
        struct pure_array arr;
        struct {
            void* data;
            pure_user_data_clean_cb clean_cb;
        } user_data;
        struct symbol_table map;
        void* opaque;
    } value;
    char buf[PURE_MAX_LOCAL_BUF_SIZE];
};

#define shared_value(x) cast(struct pure_shared_value*,(x)->value.shared_val)

#define pure_value_invalid(v) \
    do { \
        (v)->type = PURE_INVALID; \
    } while(0)

#define pure_value_number(v,n) \
    do { \
        (v)->type = PURE_NUMBER; \
        (v)->value.num = n; \
    } while(0)

#define pure_value_nil(v) \
    do { \
        (v)->type = PURE_NIL; \
    } while(0)

#define pure_value_user_data(v,sv) \
    do { \
        (v)->type = PURE_USER_DATA; \
        (v)->value.shared_val = sv; \
        shared_value(v)->ref_cnt = 1; \
    } while(0)

#define pure_value_array(v,sv) \
    do { \
        (v)->type = PURE_ARRAY; \
        (v)->value.shared_val = sv; \
        shared_value(v)->ref_cnt = 1; \
    } while(0)

#define pure_value_str(v,sv) \
    do { \
        (v)->type = PURE_STRING; \
        (v)->value.shared_val = sv; \
        shared_value(v)->ref_cnt = 1; \
    } while(0)

#define pure_value_map(v,sv) \
    do {  \
        (v)->type = PURE_MAP; \
        (v)->value.shared_val = sv; \
        shared_value(v)->ref_cnt = 1; \
    } while(0)

static
void unref_val( struct pure* f ,struct pure_value* v );

static
void ref_val( struct pure* f , struct pure_value* lhs , const struct pure_value* rhs ) {
    unref_val(f,lhs);
    lhs->type = rhs->type;
    switch( rhs->type ) {
    case PURE_NUMBER:
        lhs->value.num = rhs->value.num;
        return;
    case PURE_NIL:
        return;
    case PURE_STRING:
    case PURE_ARRAY:
    case PURE_MAP:
    case PURE_USER_DATA:
        ++(shared_value(rhs)->ref_cnt);
        lhs->value.shared_val = rhs->value.shared_val;
        return;
    default: return;
    }
}

static
int to_boolean( const struct pure_value* val ) {
    switch(val->type) {
    case PURE_ARRAY:
    case PURE_USER_DATA:
    case PURE_STRING:
        return 1;
    case PURE_NUMBER:
        return val->value.num != 0 ? 1 : 0;
    case PURE_NIL:
        return 0;
    default: return 0;
    }
}

struct pure_c_func {
    pure_cb cb;
    void* data;
};

struct pure_user_func {
    size_t loc; /* location in source */
    char par_name[PURE_MAX_LOCAL_VAR][PURE_MAX_VARNAME];
    size_t par_sz;
};

#define pure_user_func_init(c) \
    do { \
        (c)->loc = 0; \
        (c)->par_sz = 0; \
    } while(0)

struct pure_result {
    struct symbol_table user_func;
    struct symbol_table global_var;
    struct pure_user_func* cur_ufunc; /* If we are executing the code that is in a user defined function, 
                                        * then this pointer will pointed to the specific position, otherwise
                                        * it is set to NULL pointer */
    struct slab sv_slab;
    struct symbol_table stk[PURE_MAX_CALL_STACK];
    size_t cur_stk;
};

static
struct pure_result* pure_result_create() {
    struct pure_result* r = malloc(sizeof(struct pure_result));
    symbol_table_create(&(r->user_func),PURE_MAX_USER_FUNC,sizeof(struct pure_user_func));
    symbol_table_create(&(r->global_var),PURE_MAX_VALUE_SLAB,sizeof(struct pure_value));
    r->cur_ufunc = NULL;
    slab_create(&(r->sv_slab),sizeof(struct pure_shared_value),PURE_MAX_VALUE_SLAB);
    r->cur_stk = 0;
    symbol_table_init(r->stk,PURE_MAX_CALL_STACK);
    /* initialize the first stack frame */
    symbol_table_create(r->stk,PURE_MAX_LOCAL_VAR,sizeof(struct pure_value));
    return r;
}

static
void pure_result_delete( struct pure_result* p ) {
    int i;

    slab_destroy(&(p->sv_slab)); 
    symbol_table_delete(&(p->user_func));
    symbol_table_delete(&(p->global_var));
    for( i = 0 ; i  < PURE_MAX_CALL_STACK ; ++i ) {
        symbol_table_delete(p->stk+i);
    }
    free(p);
}

struct pure {
    struct symbol_table c_func;
    struct symbol_table ctx_var; /* context variable that cross different script */
    int tk; /* look ahead */
    const char* data; /* data */
    size_t loc; /* current location */
    int ec;
    char ex_err_str[1024];
    struct pure_result* cur_result;
};

enum {
    TK_EOF,TK_STR,TK_NUM,TK_VAR,TK_ASSIGN,TK_EQ,TK_NEQ,TK_LT,TK_LET,TK_GT,TK_GET,
    TK_NOT,TK_OR,TK_AND,TK_LBRA,TK_RBRA,TK_LSQR,TK_RSQR,TK_LPAR,TK_RPAR,TK_SEMICON,
    TK_ADD,TK_SUB,TK_MUL,TK_DIV,TK_MOD,TK_NIL,TK_BREAK,TK_COLON,
    TK_COMMA,TK_IF,TK_ELIF,TK_ELSE,TK_FOR,TK_LOOP,TK_FUNC,TK_RET,TK_UNKNOWN,TK_COMMENT
};


#define malloc_shared_value(f) slab_malloc(&((f)->cur_result->sv_slab))
#define free_shared_value(f,ptr) slab_free(&((f)->cur_result->sv_slab),ptr)

static
int expect_tk( struct pure * f , int tk ) {
    return f->tk == tk;
}

static
int skip( struct pure* f , int c , size_t i ) {
    for( ; f->data[i++] == c ; );
    return i-1;
}

static
int until( struct pure* f , int c , size_t i ) {
    const char* d = strchr(f->data+i,c);
    return d == NULL ? -1 : (d-f->data-i);
}

static
int var_char( int c ) {
    return isalpha(c) || c== '_' || isdigit(c);
}

static
int var_init_char( int c ) {
    return var_char(c) || isdigit(c);
}

static
void pure_array_create( struct pure_array* arr , size_t cap ) {
    arr->cap = cap;
    arr->arr = calloc(cap,sizeof(struct pure_array));
    arr->sz = 0;
}

static
void pure_array_index_assign( struct pure* f , struct pure_array* arr , int index , const struct pure_value* val ) {
    assert( index < cast(int,arr->cap) );
    ref_val(f,arr->arr+index,val);
}

static
void pure_array_grow( struct pure_array* i ) {
    /* resize of the array */
    size_t cap = i->cap * 2;
    struct pure_value* new_slot = malloc( cap*sizeof(struct pure_value) );
    memcpy(new_slot,i->arr,i->cap*sizeof(struct pure_array));
    free(i->arr);
    i->arr = new_slot;
    i->cap = cap;
}

static
void pure_array_push_back( struct pure* f , struct pure_array* i , const struct pure_value* val ) {
    if( i->sz == i->cap ) {
        pure_array_grow(i);
    }
    pure_array_index_assign(f,i,i->sz,val);
    ++i->sz;
}

static
void pure_array_remove( struct pure* f, struct pure_array* i , int index ) {
    struct pure_value* new_slot;
    struct pure_value* v;
    if( index >= cast(int,i->sz) )
        return;
    new_slot = malloc( (i->sz-1)*sizeof(struct pure_value) );
    if( index > 0 ) {
        memcpy(new_slot,i->arr,sizeof(struct pure_value)*index);
    }
    v = new_slot+4;
    if( index < cast(int,(i->sz-1)) ) {
        memcpy(new_slot+sizeof(struct pure_value)*index
            ,i->arr+sizeof(struct pure_value)*(index+1),
            sizeof(struct pure_value)*(i->sz-index-1));
    }
    unref_val(f,i->arr+index);
    i->sz--;
    i->cap = i->sz;
    free(i->arr);
    i->arr = new_slot;
}

static
struct pure_array* pure_array_fit( struct pure_array* i ) {
    struct pure_value* val = malloc(i->sz*sizeof(struct pure_value));
    memcpy(val,i->arr,sizeof(struct pure_value)*i->sz);
    i->cap = i->sz;
    free(i->arr);
    i->arr = val;
    return i;
}

static
void pure_array_delete( struct pure* f , struct pure_array* arr ) {
    size_t i;
    for( i = 0 ; i < arr->sz ; ++i ) {
        unref_val(f,arr->arr+i);
    }
    free(arr->arr);
    arr->cap = 0;
    arr->arr = NULL;
}

static
void unref_val( struct pure* f , struct pure_value* v ) {
    if(v->type == PURE_STRING || v->type == PURE_ARRAY || v->type == PURE_USER_DATA ) {
        assert(shared_value(v)->ref_cnt >0);
        --(shared_value(v)->ref_cnt);
        if( shared_value(v)->ref_cnt == 0 ) {
            switch(v->type) {
            case PURE_STRING:
                if( shared_value(v)->buf != shared_value(v)->value.str.c_str ) {
                    free(cast(void*,shared_value(v)->value.str.c_str));
                }
                v->type = PURE_INVALID;
                free_shared_value(f,v->value.shared_val);
                break;
            case PURE_ARRAY:
                pure_array_delete(f,&(shared_value(v)->value.arr));
                v->type = PURE_INVALID;
                free_shared_value(f,v->value.shared_val);
                break;
            case PURE_MAP:
                symbol_table_delete(&(shared_value(v)->value.map));
                v->type = PURE_INVALID;
                free_shared_value(f,v->value.shared_val);
                break;
            case PURE_USER_DATA:
                if( shared_value(v)->value.user_data.clean_cb )
                    shared_value(v)->value.user_data.clean_cb(shared_value(v)->value.user_data.data);
                v->type = PURE_INVALID;
                free_shared_value(f,v->value.shared_val);
                break;
            default: break;
            }
        }
    }
    v->type = PURE_INVALID;
}

static
int next_tk( struct pure* f , int offset ) {
    int ret;
    f->loc += offset;
    do {
        switch(f->data[f->loc]) {
        case 0: return (f->tk=TK_EOF);
        case '#': ret = until(f,'\n',f->loc)+1; 
            if(ret == -1) return TK_EOF;
            else { f->loc += ret; break; }
        case ' ': case '\t': case '\r':  case '\n':
            f->loc = skip(f,f->data[f->loc],f->loc); break;
        case '0':case '1':case '2':case '3':case '4':
        case '5':case '6':case '7':case '8':case '9':
            return (f->tk = TK_NUM);
        case '\"': return (f->tk = TK_STR);
        case ',' : return (f->tk = TK_COMMA);
        case '+' : return (f->tk = TK_ADD);
        case '-' : return (f->tk = TK_SUB);
        case '*' : return (f->tk = TK_MUL);
        case '/' : return (f->tk = TK_DIV);
        case '%' : return (f->tk = TK_MOD);
        case ';' : return (f->tk = TK_SEMICON);
        case ':' : return (f->tk = TK_COLON);
        case '=' : 
            return f->data[f->loc+1] == '=' ? (f->tk=TK_EQ) : (f->tk=TK_ASSIGN);
        case '!':
            return f->data[f->loc+1] == '=' ? (f->tk=TK_NEQ): (f->tk=TK_NOT);
        case '>':
            return f->data[f->loc+1] == '=' ? (f->tk=TK_GET) : (f->tk=TK_GT);
        case '<':
            return f->data[f->loc+1] == '=' ? (f->tk=TK_LET) : (f->tk=TK_LT);
        case '|':
            return f->data[f->loc+1] == '|' ? (f->tk=TK_OR) : (f->tk=TK_UNKNOWN);
        case '&':
            return f->data[f->loc+1] == '&' ? (f->tk=TK_AND): (f->tk=TK_UNKNOWN);
        case '[': return (f->tk=TK_LSQR); case ']': return (f->tk=TK_RSQR);
        case '{': return (f->tk=TK_LBRA); case '}': return (f->tk=TK_RBRA);
        case '(': return (f->tk=TK_LPAR); case ')': return (f->tk=TK_RPAR);
        default:
            switch(f->data[f->loc]) {
            case 'b': 
                if( f->data[f->loc+1] == 'r' && f->data[f->loc+2] == 'e' &&
                    f->data[f->loc+3] == 'a' && f->data[f->loc+4] == 'k' &&
                    !var_init_char(f->data[f->loc+5]) )
                    return (f->tk = TK_BREAK);
                else
                    return (f->tk = TK_VAR);
            case 'i': return f->data[f->loc+1]=='f'&& !(var_init_char(f->data[f->loc+2])) ? 
                          (f->tk = TK_IF) : (f->tk = TK_VAR);
            case 'e': 
                if( f->data[f->loc+1] == 'l' && f->data[f->loc+2] == 'i' &&
                    f->data[f->loc+3] == 'f' && !var_init_char(f->data[f->loc+4]) )
                    return (f->tk = TK_ELIF);
                else if( f->data[f->loc+1] == 'l' && f->data[f->loc+2] == 's' &&
                         f->data[f->loc+3] == 'e' && !var_init_char(f->data[f->loc+4]) )
                    return (f->tk = TK_ELSE);
                else
                    return (f->tk = TK_VAR);
            case 'r':
                if( f->data[f->loc+1] == 'e' && f->data[f->loc+2] == 't' && 
                    f->data[f->loc+3] == 'u' && f->data[f->loc+4] == 'r' &&
                    f->data[f->loc+5] == 'n' && !var_init_char(f->data[f->loc+6]) )
                    return (f->tk = TK_RET);
                else
                    return (f->tk = TK_VAR);
            case 'f':
                if( f->data[f->loc+1] == 'u' && f->data[f->loc+2] == 'n' && 
                    f->data[f->loc+3] == 'c' && !var_init_char(f->data[f->loc+4]) )
                    return (f->tk = TK_FUNC);
                else if( f->data[f->loc+1] == 'o' && 
                         f->data[f->loc+2] == 'r' && 
                         !var_init_char(f->data[f->loc+3]) )
                         return (f->tk = TK_FOR);
                else
                    return (f->tk = TK_VAR);
            case 'l':
                if( f->data[f->loc+1] == 'o' && f->data[f->loc+2] == 'o' &&
                    f->data[f->loc+3] == 'p' && !var_init_char(f->data[f->loc+4]) )
                    return (f->tk = TK_LOOP);
                else
                    return (f->tk = TK_VAR);
            case 'n':
                if( f->data[f->loc+1] == 'i' && f->data[f->loc+2] == 'l' && !var_init_char(f->data[f->loc+3]) )
                    return (f->tk = TK_NIL);
                else
                    return (f->tk = TK_VAR);
            default:
                if( var_init_char(f->data[f->loc]) )
                    return (f->tk=TK_VAR);
                return (f->tk=TK_UNKNOWN);
            }
        }
    } while(1);
}

static
void set_pc( struct pure* f , int pc ) {
    f->loc = pc;
    next_tk(f,0);
}

static  
int number( const struct pure* f , double* val ) {
    char* end;
    assert(f->tk == TK_NUM);
    errno = 0;
    *val = strtod(f->data+f->loc,&end);
    if( errno != 0 )
        return -1;
    return end- (f->data+f->loc);
}

static
int variable( const struct pure* f , char var[PURE_MAX_VARNAME] ) {
    size_t i = f->loc+1;
    size_t pos = 0;
    assert(f->tk == TK_VAR);
    var[pos++]=f->data[f->loc];
    for( ; var_char(f->data[i]) && pos < PURE_MAX_VARNAME -1 ; ++i ) 
        var[pos++] = f->data[i];
    if( pos == PURE_MAX_VARNAME -1 )
        return -1;
    var[pos] =0;
    return i-(f->loc);
}

static
int escp_char( int c ) {
    switch(c) {
    case 'n': return '\n';
    case 't': return '\t';
    case 'b': return '\b';
    case 'r': return '\r';
    default: return 0;
    }
}

static
char* str( const struct pure* f , int* offset , char buf[PURE_MAX_LOCAL_BUF_SIZE] ) {
    size_t i;
    size_t pos = 0;
    size_t sz;
    int len = 0;
    int c;

    char* ret = NULL;
    assert(f->tk == TK_STR);
    /* find out the length of the str */
    for( i = f->loc+1; f->data[i] ; ++i , ++len ) {
        if(f->data[i] == '\\' ) {
            ++i;
            if(f->data[i]==0)
                return NULL; /* error, EOF comes after the \\ */
        } else if( f->data[i] == '\"' )
            break;
    }
    if( len + 1 <= PURE_MAX_VARNAME )
        ret = buf;
    else
        ret = malloc(len+1);/* extra position for 0 */

    *offset = cast(int,(i+1)-f->loc);
    sz = i;

    for ( i = f->loc+1 , pos = 0 ; i < sz ; ++i ) {
        if(f->data[i] == '\\' ) {
            ++i;
            if( (c=escp_char(f->data[i])) ) {
                ret[pos++]=c; /* copy the escape character */
                continue;
            }
        }
        ret[pos++] = f->data[i];
    }
    ret[pos]=0;
    assert( pos == len );
    return ret;
}

static
int factor( struct pure* f , struct pure_value* val );
static 
int term( struct pure* f , struct pure_value* val );
static
int arith( struct pure* f , struct pure_value* val );
static
int comp( struct pure* f , struct pure_value* val );
static
int expr( struct pure* f , struct pure_value* val );
static
int rhs_val( struct pure* f , struct pure_value* val );
static
int map_key( struct pure* f , struct pure_value* val );
static
int interp_scope( struct pure* f , struct pure_value* val , int* ret , int* brk );
static
int interp_global( struct pure* f );
static
int exec_block( struct pure* f , struct pure_value* val , int* ret , int* brk );
static
int skip_block( struct pure* f );
static
int skip_line( struct pure* f );

static
int invoke_user_func( struct pure* f , struct pure_user_func* ufunc , const struct pure_value* par , size_t sz , struct pure_value* result ) {
    int pc = f->loc;
    size_t i;
    int is_ret = 1;
    int is_brk = 0;
    int ret;
    struct pure_value* par_val[PURE_MAX_FUNC_PAR];
    struct symbol_table* stk;
    struct pure_user_func* prev_func = f->cur_result->cur_ufunc;

    assert(f->cur_result);

    f->cur_result->cur_ufunc = ufunc; /* set the current user function */
    set_pc(f,ufunc->loc);

    /* Set up the stack */
    if(f->cur_result->cur_stk == PURE_MAX_CALL_STACK) {
        f->ec = PURE_EC_STACK_OVERFLOW;
        return -1;
    }

    /* Increase the stack pointer*/
    f->cur_result->cur_stk++;
    stk = f->cur_result->stk + f->cur_result->cur_stk;
    symbol_table_resume(stk,PURE_MAX_LOCAL_VAR,sizeof(struct pure_value));

    /* Push all the parameter on the local variable look up table (stack) */
    for( i = 0 ; i < sz ; ++i ) {
        int insert;
        struct pure_value* slot;
        slot = symbol_table_insert(stk,ufunc->par_name[i],&insert);
        if( insert ) {
            pure_value_invalid(slot);
        } 
        ref_val(f,slot,par + i);
        par_val[i] = slot;
    }

    ret = exec_block(f,result,&is_ret,&is_brk);
    
    /* Clear all the parameters */
    for( i = 0 ; i < sz ; ++i ) {
        unref_val(f,par_val[i]);
    }
    symbol_table_clean(stk);

    /* Decrease the stack pointer */
    assert(f->cur_result->cur_stk !=0);
    --(f->cur_result->cur_stk);

    /* Move back PC to the callee */
    set_pc(f,pc);
    f->cur_result->cur_ufunc = prev_func;
    return ret;
}

static
int invoke_fn( struct pure* f , const char* n , struct pure_value* par , size_t sz , struct pure_value* result ) {
    /* Invoking the function, a function _CAN_ be a user defined function or a external C function */
    struct pure_user_func* ufunc;
    ufunc = symbol_table_query(&(f->cur_result->user_func),n);
    if( ufunc != NULL ) {
        return invoke_user_func(f,ufunc,par,sz,result);
    } else {
        struct pure_c_func* cfunc;
        cfunc = symbol_table_query(&(f->c_func),n);
        if( cfunc != NULL ) {
            if( cfunc->cb(f,par,sz,result,cfunc->data) != 0 ) {
                f->ec = PURE_EC_FUNCTION_EXECUTION_ERROR;
                return -1;
            }
            return 0;
        }
        f->ec = PURE_EC_NO_SUCH_FUNC;
        return -1;
    }
}

static
void lookup_var( struct pure* f , const char var[PURE_MAX_VARNAME] , struct pure_value* ret ) {
    const struct pure_value* val = NULL;
    if( f->cur_result != NULL && f->cur_result->cur_ufunc != NULL ) {
        val = symbol_table_query(f->cur_result->stk+(f->cur_result->cur_stk),var);
    } 
    if( val == NULL ) {
        if(f->cur_result != NULL )
            val = symbol_table_query(&(f->cur_result->global_var),var);
        /* lastly we need to look it up in the context variable */
        if( val == NULL ) {
            val = symbol_table_query(&(f->ctx_var),var);
            if( val == NULL ) {
                pure_value_nil(ret);
                return;
            }
        }
    }
    ref_val(f,ret,val);
}

static
int update_var( struct pure* f , const char var[PURE_MAX_VARNAME] , const struct pure_value* val ) {
    struct pure_value* slot = NULL;
    int insert;
    if( f->cur_result != NULL && f->cur_result->cur_ufunc != NULL ) {
        slot = symbol_table_insert(f->cur_result->stk+(f->cur_result->cur_stk),var,&insert);
    }
    if( slot == NULL ) {
        if( f->cur_result != NULL )
            slot = symbol_table_insert(&(f->cur_result->global_var),var,&insert);
        if( slot == NULL ) {
            slot = symbol_table_query(&(f->ctx_var),var);
            if( slot == NULL ) {
                return -1;
            }
        }
    }
    if( insert ) {
        pure_value_invalid(slot);
    }
    ref_val(f,slot,val);
    return 0;
}

/* this function is called to execute the function and put the value into the
 * fancy_val object or it just store the var into the val object when it is 
 * not a function call */
static
int call_fn( struct pure* f , const char fn[PURE_MAX_VARNAME] , struct pure_value* ret ) {
    struct pure_value par[PURE_MAX_FUNC_PAR];
    size_t par_sz = 0;
    int r;
    size_t i;

    /* it is a function call */
    if( next_tk(f,1) != TK_RPAR ) {
        do {
            if( rhs_val(f,par+par_sz++) != 0 )
                return -1;
            if( f->tk == TK_COMMA ) {
                next_tk(f,1);
                continue;
            } else if( f->tk == TK_RPAR ) {
                next_tk(f,1);
                break;
            } else {
                f->ec = PURE_EC_EXPECT_COMMA;
                return -1;
            }

            if( par_sz == PURE_MAX_FUNC_PAR ) {
                f->ec = PURE_EC_TOO_MANY_FUNCTION_PARAMETERS;
                return -1;
            }

        } while(1);
    } else {
        next_tk(f,1);
    }

    /* call the function */
    r = invoke_fn(f,fn,par,par_sz,ret);
    for( i = 0 ; i < par_sz ; ++i ) {
        unref_val(f,par+i);
    }

    return r;
}

static
int is_int( double val ) {
    double i;
    double v = modf(val,&i);
    return i == val;
}

static
int64_t to_int( double val ) {
    double i;
    double v = modf(val,&i);
    return cast(int64_t,i);
}

static
int obj_idx_ref( struct pure* f , const char var[PURE_MAX_VARNAME] , struct pure_value** val , struct pure_array** arr_obj ) {
    struct pure_value obj;
    struct pure_value idx_val;
    size_t idx;

    assert(f->tk == TK_LSQR);

    pure_value_invalid(&idx_val);
    pure_value_invalid(&obj);

    lookup_var(f,var,&obj);
    if( arr_obj )
        *arr_obj = NULL;

    if( obj.type == PURE_ARRAY ) {
        next_tk(f,1);
        if( expr(f,&idx_val) != 0 )
            goto fail;
        if(f->tk != TK_RSQR)
            goto fail;
        next_tk(f,1); /* Consume this } */
        /* checking if it is real index */
        if( idx_val.type != PURE_NUMBER ) {
            f->ec = PURE_EC_ARRAY_INDEX_SHOULD_BE_INTEGER;
            goto fail;
        }
        /* checking if this index is integer */
        if( !is_int(idx_val.value.num) ) {
            f->ec = PURE_EC_ARRAY_INDEX_SHOULD_BE_INTEGER;
            goto fail;
        }
        idx = cast(size_t,idx_val.value.num);
        if( pure_array_index(&(shared_value(&obj)->value.arr),idx,val) != 0 ) {
            *val = NULL;
        }
        if( arr_obj ) 
            *arr_obj = &(shared_value(&obj)->value.arr);
        unref_val(f,&obj);
        return 0;
    } else if( obj.type == PURE_MAP ) {
        next_tk(f,1);
        if( expr(f,&idx_val) != 0 )
            goto fail;
        if(f->tk != TK_RSQR)
            goto fail;
        next_tk(f,1);
        /* checking if it is real index */
        if( idx_val.type != PURE_STRING ) {
            f->ec = PURE_EC_MAP_INDEX_SHOULD_BE_STRING;
            goto fail;
        }
        *val = symbol_table_query(&(shared_value(&obj)->value.map),shared_value(&idx_val)->value.str.c_str);
        unref_val(f,&obj);
        return 0;
    } else {
        f->ec = PURE_EC_INDEX_ON_NON_ARRAY_OR_MAP;
        goto fail;
    }
fail:
    unref_val(f,&obj);
    unref_val(f,&idx_val);
    return -1;
}

/* var_name[EXP] */
static
int obj_idx( struct pure* f , const char var[PURE_MAX_VARNAME] , struct pure_value* val ) {
    struct pure_value* tar;
    assert(f->tk == TK_LSQR);
    if( obj_idx_ref(f,var,&tar,NULL) !=0 )
        return -1;
    else {
        if( tar != NULL )
            ref_val(f,val,tar);
        else
            pure_value_nil(val);
        return 0;
    }
}

static
int var_or_call_fn( struct pure* f , const char var[PURE_MAX_VARNAME] , struct pure_value* val ) {
    switch(f->tk) {
    case TK_LPAR:
        return call_fn(f,var,val);
    case TK_LSQR:
        return obj_idx(f,var,val);
    default:
        lookup_var(f,var,val); return 0;
    }
}

static
int const_fn( struct pure* f , struct pure_value* val ) {
    double num;
    const char* s;
    char var[PURE_MAX_LOCAL_BUF_SIZE];
    int ret;
    struct pure_shared_value* sv;

    switch(f->tk) {
    case TK_LPAR: 
        next_tk(f,1); ret = expr(f,val); 
        if(ret !=0) return ret; 
        else { next_tk(f,1); return 0; }
    case TK_NUM : 
        if( (ret = number(f,&num)) <0 ) {
            f->ec = PURE_EC_UNKNOWN_NUMBER;
            return -1;
        } else {
            pure_value_number(val,num); next_tk(f,ret); return 0;
        }
    case TK_STR:
        sv = malloc_shared_value(f);
        if( (s = str(f,&ret,sv->buf)) == NULL ) {
            f->ec = PURE_EC_UNKNOWN_STRING_LITERAL;
            free_shared_value(f,sv);
            return -1;
        } else {
            /* have to compose it manually since str function return heap buffer */
            val->type = PURE_STRING;
            pure_value_str(val,sv);
            /* assign the string value to the slots */
            sv->value.str.c_str = s;
            sv->value.str.sz = strlen(s);
            next_tk(f,ret); return 0;
        }
    case TK_VAR:
        if( (ret = variable(f,var)) < 0 ) {
            f->ec = PURE_EC_UNKNOWN_VARIABLE;
            return -1;
        } else {
            next_tk(f,ret);
            return var_or_call_fn(f,var,val);
        }
    case TK_NIL:
        next_tk(f,3);
        pure_value_nil(val);
        return 0;
    default: 
        f->ec = PURE_EC_UNKNOWN_CONSTANT_SYMBOL;
        return -1;
    }

}

static
int factor( struct pure* f , struct pure_value* val ) {
    int tk;
#define DO() \
    do { \
        tk = f->tk; \
        next_tk(f,1); \
        if( expr(f,val) != 0 ) \
            return -1; \
    } while(0)


    switch(f->tk) {
    case TK_ADD:
        DO();
        return 0;

    case TK_NOT:
        DO();
        if( val->type == PURE_NIL ) {
            unref_val(f,val);
            pure_value_number(val,1);
        } else if( val->type == PURE_NUMBER ) {
            if(val->value.num == 0) {
                unref_val(f,val);
                pure_value_number(val,1);
            } else {
                unref_val(f,val);
                pure_value_number(val,0);
            }
        } else {
            pure_value_number(val,0);
        }
        return 0;
    case TK_SUB:
        DO();
        if( val->type == PURE_NUMBER ) {
            pure_value_number(val,-val->value.num);
            return 0;
        } else {
            f->ec = PURE_EC_UNKNOWN_UNARY_OPRAND;
            return -1;
        }
#undef DO
    default:
        return const_fn(f,val);
    }
}

static 
int term( struct pure* f , struct pure_value* val ) {
    struct pure_value top;
    int op;

    pure_value_invalid(&top);

    if( factor(f,val) != 0 )
        return -1;
    else {
        do {
            switch(f->tk) {
            case TK_MUL:
            case TK_DIV:
            case TK_MOD:
                op = f->tk;
                next_tk(f,1);
                if( factor(f,&top) != 0 )
                    return -1;
                if(top.type == PURE_NUMBER && val->type == PURE_NUMBER) {
                    switch(op) {
                    case TK_MUL:
                        pure_value_number(val,top.value.num*val->value.num);
                        break;
                    case TK_DIV:
                        if( top.value.num == 0 ) {
                            f->ec = PURE_EC_DIV_ZERO;
                            return -1;
                        }
                        pure_value_number(val,val->value.num/top.value.num);
                        break;
                    case TK_MOD:
                        if( top.value.num == 0 ) {
                            f->ec = PURE_EC_DIV_ZERO;
                            return -1;
                        }
                        pure_value_number(val,
                            cast(double,to_int(val->value.num)%to_int(top.value.num)));
                        break;
                    default: break;
                    }

                } else {
                    f->ec = PURE_EC_ONLY_NUMBER_CAN_BE_FACTOR;
                    return -1;
                }
                break;
            default:
                return 0;
            }
        } while(1);
    }
}

static
int arith( struct pure* f , struct pure_value* val ) {
    struct pure_value top;
    pure_value_invalid(&top);

    if( term(f,val) !=0 )
        return -1;
    else {
        int op;
        do {
            switch(f->tk) {
            case TK_ADD:
            case TK_SUB:
                op = f->tk;
                next_tk(f,1);
                if( term(f,&top) != 0 )
                    return -1;

                if( val->type == PURE_NUMBER && top.type == PURE_NUMBER ) {
                    if( op == TK_ADD )
                        pure_value_number(val,val->value.num+top.value.num);
                    else
                        pure_value_number(val,val->value.num-top.value.num);
                } else {
                    f->ec = PURE_EC_ONLY_NUMBER_CAN_BE_TERM;
                    return -1;
                }
                break;
            default:
                return 0;
            }

        } while(1);
    }
}

static
void str_cmp( struct pure* f , int op , struct pure_value* l , struct pure_value* r ) {

#define DO(op) \
    do { \
        int val = strcmp(shared_value(l)->value.str.c_str,shared_value(r)->value.str.c_str) op 0 ? 1 : 0; \
        unref_val(f,l); \
        unref_val(f,r); \
        pure_value_number(l,val); \
        return; \
    } while(0)

    switch(op) {
    case TK_LT: 
        DO(<);
    case TK_LET:
        DO(<=);
    case TK_EQ: 
        DO(==);
    case TK_NEQ:
        DO(!=);
    case TK_GT: 
        DO(>);
    case TK_GET:
        DO(>=);
    default: assert(0);
    }

#undef DO
}

static
void num_cmp( int op , struct pure_value* l , struct pure_value* r ) {
    switch(op) {
    case TK_LT: 
        pure_value_number(l,l->value.num<r->value.num); break;
    case TK_LET:
        pure_value_number(l,l->value.num<=r->value.num); break;
    case TK_EQ: 
        pure_value_number(l,l->value.num==r->value.num); break;
    case TK_NEQ:
        pure_value_number(l,l->value.num!=r->value.num); break;
    case TK_GT: 
        pure_value_number(l,l->value.num>r->value.num); break;
    case TK_GET:
        pure_value_number(l,l->value.num>=r->value.num); break;
    default: assert(0);
    }
}

static
int comp( struct pure* f , struct pure_value* val) {
    struct pure_value top;

    if( arith(f,val) != 0 )
        return -1;
    else {
        int op;

#define DO(a,b) op = (a); next_tk(f,(b)); break
        do {
            switch(f->tk) {
            case TK_LT: DO(TK_LT,1);
            case TK_LET:DO(TK_LET,2); 
            case TK_EQ: DO(TK_EQ,2); 
            case TK_NEQ:DO(TK_NEQ,2); 
            case TK_GT: DO(TK_GT,1); 
            case TK_GET:DO(TK_GET,2); 
            default:
                return 0;
            }

            if( arith(f,&top) < 0 )
                return -1;
            if( top.type == PURE_NUMBER && val->type == PURE_NUMBER ) {
                num_cmp(op,val,&top);
            } else if( top.type == PURE_STRING && val->type == PURE_STRING ) {
                str_cmp(f,op,val,&top);
            } else {
                switch(op) {
                case TK_EQ:
                    if( top.type == PURE_NIL && val->type == PURE_NIL ) {
                        pure_value_number(val,1);
                    } else {
                        if( top.type == PURE_NIL || val->type == PURE_NIL ) {
                            pure_value_number(val,0);
                        } else {
                            goto fail;
                        }
                    }
                    break;
                case TK_NEQ:
                    if( top.type != val->type ) {
                        pure_value_number(val,1);
                    } else {
                        if( top.type == PURE_NIL && val->type == PURE_NIL )
                            pure_value_number(val,0);
                        else
                            goto fail;
                    }
                    break;
                default:
                    break;
                }
            }
        } while(1);
#undef DO
    }

fail:
    unref_val(f,&top);
    f->ec = PURE_EC_TYPE_ERROR_FOR_COMPARTOR;
    return -1;
}

static
int expr( struct pure* f , struct pure_value* val ) {
    struct pure_value top;
    pure_value_invalid(&top);

#define DO(x,v) \
    do { \
    if((x)->type == PURE_NIL) \
    v = 0; \
        else if( (x)->type == PURE_NUMBER ) \
        v = cast(int,(x)->value.num); \
        else \
        v = 1; \
    } while(0)

    if( comp(f,val) != 0 )
        return -1;
    else {
        int op;
        int lval, rval;

        do {
            switch(f->tk) {
            case TK_AND:
            case TK_OR:
                op = f->tk;
                next_tk(f,2);
                break;
            default:
                return 0;
            }

            if( comp(f,&top) != 0 ) {
                unref_val(f,val);
                unref_val(f,&top);
                return -1;
            }

            DO(val,lval);
            DO(&top,rval);

            unref_val(f,val);
            unref_val(f,&top);

            switch(op) {
            case TK_AND:
                pure_value_number(val,lval&&rval); 
                return 0;
            case TK_OR:
                pure_value_number(val,lval||rval);
                return 0;
            default: return 0;
            }
        } while(1);
    }

#undef DO

}

static
int def_array( struct pure* f , struct pure_value* val ) {
    struct pure_value top;
    struct pure_shared_value* sv;
    int empty = 0;

    assert( f->tk == TK_LSQR );
    sv = malloc_shared_value(f);
    pure_array_create(&(sv->value.arr),PURE_MAX_ARRAY_LEN);
    pure_value_invalid(&top);
    next_tk(f,1);
    if( f->tk != TK_RSQR ) {
        do {
            if( rhs_val(f,&top) !=0 ) {
                goto fail;
            } 
            pure_array_push_back(f,&(sv->value.arr),&top);
            if( f->tk == TK_COMMA ) {
                next_tk(f,1);
            } else if( f->tk == TK_RSQR ) {
                next_tk(f,1);
                break;
            } else {
                f->ec = PURE_EC_ARRAY_EXPECT_RIGHT_SQURA;
                goto fail;
            }
        } while(1);
    } else {
        empty = 1;
        next_tk(f,1);
    }

    /* success eval the array */
    val->type = PURE_ARRAY;
    if( !empty )
        pure_array_fit(&(sv->value.arr));
    pure_value_array(val,sv);
    return 0;

fail:
    pure_array_delete(f,&(sv->value.arr));
    free_shared_value(f,sv);
    return -1;
}

/* { Key1:Value1 , Key2:Value2 } */
static
int map_key( struct pure* f , struct pure_value* val ) {
    char ret[PURE_MAX_VARNAME];
    int offset;
    char* s;
    struct pure_shared_value* sv;

    switch(f->tk) {
    case TK_VAR:
        if( (offset=variable(f,ret)) <0 ) {
            f->ec = PURE_EC_UNKNOWN_VARIABLE;
            return -1;
        } else {
            next_tk(f,offset);
            return var_or_call_fn(f,ret,val);
        }
    case TK_STR:
        sv = malloc_shared_value(f);
        if( (s=str(f,&offset,sv->buf)) == NULL ) {
            f->ec = PURE_EC_UNKNOWN_STRING_LITERAL;
            free_shared_value(f,sv);
            return -1;
        } else {
            next_tk(f,offset);
            sv = malloc_shared_value(f);
            sv->value.str.c_str = s;
            sv->value.str.sz = strlen(s);
            pure_value_str(val,sv);
            return 0;
        }
    default: f->ec = PURE_EC_UNKNOWN_MAP_KEY; return -1;
    }
}

static
int map_kv( struct pure* f , struct pure_value* key , struct pure_value* val ) {
    if( map_key(f,key) != 0 )
        return -1;
    if( f->tk != TK_COLON ) {
        f->ec = PURE_EC_EXPECT_COLON;
        return -1;
    }
    next_tk(f,1);
    if( rhs_val(f,val) != 0 )
        return -1;
    return 0;
}

static
int def_map( struct pure* f , struct pure_value* val ) {
    struct pure_shared_value* sv;
    struct pure_value k,v;
    struct pure_value* slot;
    int insert;

    assert(f->tk == TK_LBRA);
    pure_value_invalid(&k);
    pure_value_invalid(&v);

    sv = malloc_shared_value(f);
    symbol_table_create(&(sv->value.map),PURE_MAX_USER_MAP_SIZE,sizeof(struct pure_value));

    if( next_tk(f,1) != TK_RBRA ) {
        while(1) {

            if( map_kv(f,&k,&v) != 0 )
                goto fail;

            if(k.type != PURE_STRING) {
                f->ec = PURE_EC_MAP_INDEX_SHOULD_BE_STRING;
                goto fail;
            }

            slot = symbol_table_insert(
                &(sv->value.map),shared_value(&k)->value.str.c_str,&insert);

            /* insert the value into the table */
            if( insert ) {
                *slot = v;
            } else {
                ref_val(f,slot,&v);
                unref_val(f,&v);
                unref_val(f,&k);
            }

            if( f->tk == TK_COMMA ) {
                next_tk(f,1);
            } else if( f->tk == TK_RBRA ) {
                next_tk(f,1);
                break;
            } else {
                f->ec = PURE_EC_EXPECT_COMMA;
                goto fail;
            }
        }
    } else {
        /* consume the } for the empty map */
        next_tk(f,1);
    }

    pure_value_map(val,sv);
    return 0;

fail:
    unref_val(f,&k);
    unref_val(f,&v);
    symbol_table_delete(&(sv->value.map));
    free_shared_value(f,sv);
    return -1;
}

static
int rhs_val( struct pure* f , struct pure_value* val ) {
    switch(f->tk) {
    case TK_LSQR:
        return def_array(f,val);
    case TK_LBRA:
        return def_map(f,val);
    default:
        return expr(f,val);
    }
}

static
int assign_or_call_fn( struct pure* f ) {
    char var[PURE_MAX_VARNAME];
    int ret;
    int offset;
    struct pure_value val;
    struct pure_value* idx;
    struct pure_array* arr;

    pure_value_invalid(&val);
    
    assert(f->tk == TK_VAR);

    if( (offset=variable(f,var)) <= 0 ) {
        f->ec = PURE_EC_UNKNOWN_VARIABLE;
        return -1;
    }
    next_tk(f,offset);
    /* checking it is a function call or a assignment */
    switch(f->tk) {
    case TK_LPAR:
        if( (ret = call_fn(f,var,&val)) !=0 )
            unref_val(f,&val);
        goto done;
    case TK_LSQR: /* We allow indexer as left hand side value */
        ret = obj_idx_ref(f,var,&idx,&arr);
        if( ret != 0 ) {
            goto done;
        } else if( idx == NULL ) {
            f->ec = PURE_EC_NO_SUCH_FILE;
            goto done;
        }

        if(f->tk != TK_ASSIGN) {
            f->ec = PURE_EC_SYNTAX_ERROR;
            goto done;
        }
        next_tk(f,1);
        if( (ret=rhs_val(f,&val)) !=0) {
            goto done;
        }

        /* For map and array , if we assign the nil value, 
         * we have different semantics here, a map can get 
         * a nil value to indicate that slot is empty, however
         * array cannot. So we need to remove that slot in the
         * array when user assign nil to the slot of array */
        
        if( arr != NULL && val.type == PURE_NIL ) {
            /* we just need to remove that array entry here */
            int i = idx - (arr->arr);
            pure_array_remove(f,arr,i);
        } else {
            /* do normal reference and dereference */
            ref_val(f,idx,&val);
            unref_val(f,&val);
        }
        break;
    case TK_ASSIGN: /* Typical assignment, starts with a variable */
        next_tk(f,1);
        if( (ret=rhs_val(f,&val)) != 0)
            goto done;
        update_var(f,var,&val);
        unref_val(f,&val);
        break;
    default: 
        f->ec = PURE_EC_UNKNOWN_LEFT_HANDSIDE_VALUE;
        return -1;
    }
done:
    if( ret == 0 && f->tk != TK_SEMICON ) {
        f->ec = PURE_EC_EXPECT_SEMICON;
        return -1;
    }
    if( ret == 0 )
        next_tk(f,1);
    return ret;
}

/* ====================================================
 * evaluate control structure: for/loop/if-elif-else 
 * ===================================================*/


/* for :
 * 1) for( var , array ) { }
 * 2) for( var , start , end , step ) { }
 */

static
int ctrl_foreach_arr( struct pure* f , char var[PURE_MAX_VARNAME] , struct pure_value* arr , 
                  struct pure_value * ret , int* ret_back , int* brk ) {
    size_t i ;
    struct pure_array* a = &(shared_value(arr)->value.arr);
    int pc = f->loc;
    int is_ret = *ret_back;
    int is_brk = *brk;
    int exec= 0;

    *brk = *ret_back = 0;

    for( i = 0 ; i < a->sz ; ++i ) {
        exec = 1;

        set_pc(f,pc);
        update_var(f,var,a->arr+i);
        *ret_back = is_ret;
        *brk = is_brk;

        if( exec_block(f,ret,ret_back,brk) != 0 ) {
            unref_val(f,arr);
            return -1;
        }
        if( *ret_back || *brk ) {
            unref_val(f,arr);
            return 0;
        }
    }

    if(!exec) {
        /* if the loop body haven't been executed the current loc of pure is still
         * pointed to the beginning of the code block, we need to skip this block */
        if( f->tk != TK_LBRA || skip_block(f) != 0 ) {
            f->ec = PURE_EC_SYNTAX_ERROR;
            unref_val(f,arr);
            return -1;
        }
    }

    unref_val(f,arr);
    return 0;
}

static
int ctrl_foreach_map( struct pure* f , char k_var[PURE_MAX_VARNAME] , char v_var[PURE_MAX_VARNAME] , 
                      struct pure_value* map , struct pure_value* ret , int* ret_back , int* brk ) {
    struct symbol_table* a = &(shared_value(map)->value.map);
    struct pure_value* v;
    const char* n;
    struct pure_value pv_name;
    struct pure_shared_value* sv;

    int pc = f->loc;
    int is_ret = *ret_back;
    int is_brk = *brk;
    int cursor = symbol_table_iter_start(a,cast(void**,&v),&n);
    int exec= 0;

    pure_value_invalid(&pv_name);
    sv = malloc_shared_value(f);
    *brk = *ret_back = 0;

    while( symbol_table_iter_has_next(a,cursor) ) {
        /* We use NIL to indicate the remove inside of the table */
        if(v->type == PURE_NIL) {
            cursor = symbol_table_iter_deref(a,cursor,cast(void**,&v),&n);
            continue;
        }

        exec = 1;

        set_pc(f,pc);
        /* populate the shared value , since the map returns the const char*, we
         * need a pure_value object for adaption here */
        sv->value.str.sz = strlen(n);
        if( sv->value.str.sz >= PURE_MAX_LOCAL_BUF_SIZE ) {
            sv->value.str.c_str = STRDUP(n);
        } else {
            sv->value.str.c_str = strcpy(sv->buf,n);
        }
        pure_value_str(&pv_name,sv);

        /* Update the user current stack frame */
        update_var(f,k_var,&pv_name);
        update_var(f,v_var,v);
        /* Call the user callback function */
        if( exec_block(f,ret,ret_back,brk) != 0 ) {
            unref_val(f,&pv_name);
            unref_val(f,v);
            return -1;
        }

        if( *ret_back || *brk ) {
            unref_val(f,&pv_name);
            unref_val(f,v);
            return 0;
        }

        /* move forward the cursor */
        cursor = symbol_table_iter_deref(a,cursor,cast(void**,&v),&n);
    }

    if(!exec) {
        /* if the loop body haven't been executed the current loc of pure is still
         * pointed to the beginning of the code block, we need to skip this block */
        if( f->tk != TK_LBRA || skip_block(f) != 0 ) {
            f->ec = PURE_EC_SYNTAX_ERROR;
            unref_val(f,&pv_name);
            unref_val(f,v);
            return -1;
        }
    }

    return 0;
}

static
int ctrl_classic_for( struct pure* f , char var[PURE_MAX_VARNAME] , int64_t start , int64_t end , int64_t step , 
                      struct pure_value * ret , int* ret_back , int* brk ) {
    int pc = f->loc;
    int is_ret = *ret_back;
    int is_brk = *brk;
    int exec = 0;

    for( ; start < end ; start += step ) {
        struct pure_value val;
        exec = 1;
        set_pc(f,pc);

        pure_value_number(&val,cast(double,start));
        update_var(f,var,&val);

        *ret_back = is_ret;
        *brk = is_brk;

        if( exec_block(f,ret,ret_back,brk) != 0 ) {
            return -1;
        }
        if( *ret_back || *brk ) {
            return 0;
        }
    }

    if(!exec) {
        /* if the loop body haven't been executed the current loc of pure is still
         * pointed to the beginning of the code block, we need to skip this block */
        if( f->tk != TK_LBRA || skip_block(f) != 0 ) {
            f->ec = PURE_EC_SYNTAX_ERROR;
            return -1;
        }
    }

    return 0;
}

static
int ctrl_for_var( struct pure* f , char k_var[PURE_MAX_VARNAME], char v_var[PURE_MAX_VARNAME] , int* type ) {
    int offset;

    if(f->tk != TK_VAR) {
        f->ec = PURE_EC_FOR_LOOP_EXPECT_VAR;
        return -1;
    } 
    if( (offset=variable(f,k_var)) <=0 ) {
        f->ec = PURE_EC_FOR_LOOP_EXPECT_VAR;
        return -1;
    } 
    next_tk(f,offset);
    if( f->tk != TK_COLON ) {
        *type =0;
        return 0;
    } else {
        next_tk(f,1);
        if( (offset=variable(f,v_var)) <=0 ) {
            f->ec = PURE_EC_FOR_LOOP_EXPECT_VAR;
            return -1;
        }
        next_tk(f,offset);
        *type = 1;
        return 0;
    }

}

static
int ctrl_for( struct pure* f , struct pure_value* ret , int* ret_back , int* brk ) {
    char k_var[PURE_MAX_VARNAME];
    char v_var[PURE_MAX_VARNAME];

    struct pure_value par2,par3,par4;
    int tp;

    assert(f->tk == TK_FOR);
    pure_value_invalid(&par2);
    pure_value_invalid(&par3);
    pure_value_invalid(&par4);
    *brk = 1; /* loop allow break */

    next_tk(f,3);
    if( f->tk != TK_LPAR ){
        f->ec = PURE_EC_SYNTAX_ERROR;
        goto fail;
    }
    /* 0. Skip the left par */
    next_tk(f,1);
    if( ctrl_for_var(f,k_var,v_var,&tp) != 0 ) {
        goto fail;
    }
    /* 1. Second arg */
    if( f->tk != TK_COMMA ) {
        f->ec = PURE_EC_SYNTAX_ERROR;
        goto fail;
    }
    next_tk(f,1);
    if( rhs_val(f,&par2) != 0 )
        goto fail;

    switch(par2.type) {
    case PURE_ARRAY:
    case PURE_MAP:
        if( f->tk != TK_RPAR ) {
            f->ec = PURE_EC_SYNTAX_ERROR;
            goto fail;
        }
        next_tk(f,1);
        /* check the first '{' */
        if( f->tk != TK_LBRA ) {
            f->ec = PURE_EC_SYNTAX_ERROR;
            goto fail;
        }
        /* arr/map foreach */
        if( par2.type == PURE_ARRAY )
            return ctrl_foreach_arr(f,k_var,&par2,ret,ret_back,brk);
        else
            return ctrl_foreach_map(f,k_var,v_var,&par2,ret,ret_back,brk);
    /* classical for loop */
    case PURE_NUMBER:
        if( f->tk != TK_COMMA )
            goto fail;
        next_tk(f,1);
        if( rhs_val(f,&par3) != 0 || par3.type != PURE_NUMBER )
            goto fail;
        if( f->tk != TK_COMMA )
            goto fail;
        next_tk(f,1);
        if( rhs_val(f,&par4) != 0 || par4.type != PURE_NUMBER )
            goto fail;
        /* skip ) */
        if( f->tk != TK_RPAR ) {
            f->ec = PURE_EC_SYNTAX_ERROR;
            goto fail;
        }
        next_tk(f,1);
        /* check the first '{' */
        if( f->tk != TK_LBRA ) {
            f->ec = PURE_EC_SYNTAX_ERROR;
            goto fail;
        }
        return ctrl_classic_for(f,k_var,cast(int64_t,par2.value.num),
                                      cast(int64_t,par3.value.num),
                                      cast(int64_t,par4.value.num),
                                ret,ret_back,brk);
    default:
        f->ec = PURE_EC_INVALID_FOR_LOOP;
        goto fail;
    }
fail:
    unref_val(f,&par2);
    unref_val(f,&par3);
    unref_val(f,&par4);
    return -1;
}

/* loop :
 * loop(cond) { }
 */

static
int ctrl_loop( struct pure* f , struct pure_value* ret , int* ret_back , int* brk ) {
    struct pure_value cond;
    int pc;
    int is_brk = *brk;
    int is_ret = *ret_back;

    *brk = 1; /* loop allow break */
    pure_value_invalid(&cond);
    assert(f->tk == TK_LOOP);
    next_tk(f,4);

    if( f->tk != TK_LPAR ) {
        f->ec = PURE_EC_SYNTAX_ERROR;
        return -1;
    }
    next_tk(f,1);
    pc = f->loc; /* here we set the PC to just before the condition */

    while( 1 ) {
        /* cond) */
        if( rhs_val(f,&cond) != 0 ) {
            unref_val(f,&cond);
            return -1;
        }
        /* ) */
        if( f->tk != TK_RPAR ) {
            f->ec = PURE_EC_SYNTAX_ERROR;
            unref_val(f,&cond);
            return -1;
        }
        next_tk(f,1);

        /* Just check the { and left it to the exec_block */
        if( f->tk != TK_LBRA ) {
            f->ec = PURE_EC_SYNTAX_ERROR;
            return -1;
        }

        if( to_boolean(&cond) ) {

            *ret_back = is_ret;
            *brk = is_brk;

            if( exec_block(f,ret,ret_back,brk) != 0 ) {
                unref_val(f,&cond);
                return -1;
            }
            if( *ret_back || *brk ) {
                unref_val(f,&cond);
                return 0;
            }
        } else {
            if( skip_block(f) != 0 ) {
                f->ec = PURE_EC_SYNTAX_ERROR;
                return -1;
            }

            *brk = *ret_back = 0;
            unref_val(f,&cond);
            return 0;
        }

        /* Set the pc just before the condition */
        set_pc(f,pc);
    }
}

/* if-elif-else 
 * if ( cond ) { }
 * [elif(cond) { } ]
 * [else { } ]
 */

static
int ctrl_cond( struct pure* f , struct pure_value* ret , int* ret_back , int* brk ) {
    int exec = 0;
    struct pure_value cond;
    int is_ret = *ret_back;
    int is_brk = *brk;

    pure_value_invalid(&cond);
    assert(f->tk == TK_IF);
    next_tk(f,2);
    /* ( */
    if(f->tk != TK_LPAR){
        f->ec = PURE_EC_SYNTAX_ERROR;
        return -1;
    }
    next_tk(f,1);

    if(rhs_val(f,&cond) !=0)
        return -1;
    /* ) */
    if(f->tk != TK_RPAR) {
        f->ec = PURE_EC_SYNTAX_ERROR;
        unref_val(f,&cond);
        return -1;
    }
    next_tk(f,1);

    
#define DO(name,skip) \
    do { \
        if( to_boolean(&cond) ) { \
        exec = 1; \
        if( name(f,ret,ret_back,brk) != 0 ) { \
            unref_val(f,&cond); \
            return -1; \
        } \
        unref_val(f,&cond); /* clear cond */ \
        if(*brk || *ret_back) { \
            return 0; \
        } \
        } else { \
            *brk = *ret_back = 0; \
            if( skip(f) != 0 ) { \
                return -1; \
            } \
        } \
    }while(0)

    if(f->tk == TK_LBRA) {
        DO(exec_block,skip_block);
    } else {
        DO(interp_scope,skip_line);
    }

    /* Check if we have any else/elif */
    do {
        switch(f->tk) {
        case TK_ELIF:
            next_tk(f,4);
            if(f->tk != TK_LPAR){
                f->ec = PURE_EC_SYNTAX_ERROR;
                return -1;
            }
            next_tk(f,1);
            if(rhs_val(f,&cond) !=0)
                return -1;
            if(f->tk != TK_RPAR) {
                f->ec = PURE_EC_SYNTAX_ERROR;
                unref_val(f,&cond);
                return -1;
            }
            next_tk(f,1);

            if(f->tk == TK_LBRA) {
                DO(exec_block,skip_block);
            } else {
                DO(interp_scope,skip_line);
            }

            break;
        case TK_ELSE:
            next_tk(f,4); /* else */
            if(f->tk == TK_LBRA) {
                if(!exec) {
                    *ret_back = is_ret;
                    *brk = is_brk;

                    if( exec_block(f,ret,ret_back,brk) != 0 ) {
                        return -1;
                    }
                    if(*brk || *ret_back) {
                        return 0;
                    }
                } else {
                    *brk = *ret_back = 0;
                    if( skip_block(f) != 0 ) {
                        f->ec = PURE_EC_SYNTAX_ERROR;
                        return -1;
                    }
                }
            } else {
                if(!exec) {
                    *ret_back = is_ret;
                    *brk = is_brk;

                    if( interp_scope(f,ret,ret_back,brk) != 0 ) {
                        return -1;
                    }
                    if(*brk || *ret_back) {
                        return 0;
                    }
                } else {
                    *brk = *ret_back = 0;
                    if( skip_line(f) != 0 ) {
                        return -1;
                    }
                }
            }
            return 0;
        default: return 0;
        }
    } while(1);

    return 0;
}
#undef DO

/* ret
 * 1. ret expression;
 * 2. ret ;
 */

static
int ctrl_ret( struct pure* f , struct pure_value* ret ) {
    pure_value_invalid(ret);

    assert(f->tk == TK_RET);
    next_tk(f,6);

    /* Check if we have a pending expression or not */
    if( f->tk == TK_SEMICON ) {
        next_tk(f,1);
        return 0;
    } else {
        if( rhs_val(f,ret) != 0 )
            return -1;
        if( f->tk == TK_SEMICON ) {
            next_tk(f,1);
        } else {
            unref_val(f,ret);
            f->ec = PURE_EC_EXPECT_SEMICON;
            return -1;
        }
        return 0;
    }
}

/* parse function definition */
static
int def_fn( struct pure* f ) {
    int offset;
    struct pure_user_func* func;
    char fname[PURE_MAX_VARNAME];
    int insert;

    assert(f->tk == TK_FUNC);
    next_tk(f,4);

    /* function name */
    if(f->tk != TK_VAR)
        return -1;
    if( (offset = variable(f,fname)) <= 0 )
        return -1;
    next_tk(f,offset);
    func = symbol_table_insert(&(f->cur_result->user_func),fname,&insert);
    if( !insert ) {
        f->ec = PURE_EC_FUNCTION_REDEFINE;
        return -1;
    }
    pure_user_func_init(func);

    /* function parameters */
    if(f->tk != TK_LPAR)
        return -1;
    next_tk(f,1);
    if( f->tk == TK_RPAR ) {
        /* empty function parameter list */
        next_tk(f,1);
        goto done;
    }

    do {
        if(f->tk != TK_VAR || (offset=variable(f,fname)) <=0)
            return -1;
        next_tk(f,offset);
        if( func->par_sz == PURE_MAX_FUNC_PAR ) {
            f->ec = PURE_EC_TOO_MANY_FUNCTION_PARAMETERS;
            return -1;
        }
        strcpy(func->par_name[func->par_sz],fname);
        ++func->par_sz;

        if(f->tk == TK_COMMA)
            next_tk(f,1);
        else if( f->tk == TK_RPAR ) {
            next_tk(f,1);
            break; /* finish */
        } else {
            f->ec = PURE_EC_EXPECT_COMMA;
            return -1;
        }
    } while(1);
    
done:
    /* Eat a start '{' and place the function first PC right after '{' */
    if(f->tk != TK_LBRA) {
        f->ec = PURE_EC_FUNCTION_NO_BODY;
        return -1;
    }
    func->loc = f->loc;
    /* Skip to the } and resume the code */
    if( skip_block(f) != 0 ) {
        f->ec = PURE_EC_SYNTAX_ERROR;
        return -1;
    }
    return 0;
}

/* Only assignment and function definition is allowed in the global scope */
static
int interp_global( struct pure* f ) {
    switch( f->tk ) {
    case TK_VAR:
        return assign_or_call_fn(f);
    case TK_FUNC:
        return def_fn(f);
    default: 
        f->ec = PURE_EC_GLOBAL_SCOPE_SYNTAX_ERROR; 
        return -1;
    }
}

static 
int interp_scope( struct pure* f , struct pure_value* val , int* ret , int* brk ) {
    switch(f->tk) {
    case TK_VAR:
        *brk = *ret = 0;
        return assign_or_call_fn(f);
    case TK_IF:
        return ctrl_cond(f,val,ret,brk);
    case TK_FOR:
        return ctrl_for(f,val,ret,brk);
    case TK_LOOP:
        return ctrl_loop(f,val,ret,brk);
    case TK_RET:
        *ret = 1;
        return ctrl_ret(f,val);
    case TK_BREAK:
        *brk = 1;
        next_tk(f,5);
        if(f->tk != TK_SEMICON) {
            f->ec = PURE_EC_EXPECT_SEMICON;
            return -1;
        }
        next_tk(f,1);
        return 0;
    default: f->ec = PURE_EC_SYNTAX_ERROR; return -1;
    }
}

static
int exec_block( struct pure* f , struct pure_value* val , int* ret , int* brk ) {
    /* Execute the code until we reach a } */
    int is_ret = *ret;
    int is_brk = *brk;
    assert(f->tk == TK_LBRA);
    next_tk(f,1);
    do {
        if( interp_scope(f,val,ret,brk) != 0 )
            return -1;
        /* We stop the execution when we meet the return/break */
        if( is_ret && *ret )
            return 0;
        if( is_brk && *brk )
            return 0;
    } while(f->tk != TK_RBRA);
    next_tk(f,1);
    return 0;
}

static
int skip_line( struct pure* f ) {
    char buf[PURE_MAX_LOCAL_BUF_SIZE];
    const char* s;
    int offset;
    double num;
    int tk = next_tk(f,0);
    do {
        switch(tk) {
        case TK_EOF:
            return -1;
        case TK_STR:
            s = str(f,&offset,buf);
            if(s == NULL)
                return -1;
            if(s != buf)
                free(cast(void*,s));
            tk = next_tk(f,offset);
            break;
        case TK_NUM:
            if((offset=number(f,&num)) <=0)
                return -1;
            tk = next_tk(f,offset);
            break;
        case TK_VAR:
            if((offset=variable(f,buf)) <=0)
                return -1;
            tk = next_tk(f,offset);
            break;
        case TK_LBRA:
            tk = next_tk(f,1);
            break;
        case TK_RBRA:
            tk = next_tk(f,1);
            break;
        case TK_SEMICON:
            tk = next_tk(f,1);
            return 0;

    #define DO(t,o) \
        case t: \
        tk = next_tk(f,o); \
        break

            DO(TK_EQ,2);
            DO(TK_NEQ,2);
            DO(TK_LT,1);
            DO(TK_LET,2);
            DO(TK_GT,1);
            DO(TK_GET,2);
            DO(TK_NOT,1);
            DO(TK_OR,2);
            DO(TK_AND,2);
            DO(TK_LSQR,1);
            DO(TK_RSQR,1);
            DO(TK_LPAR,1);
            DO(TK_RPAR,1);
            DO(TK_ADD,1);
            DO(TK_SUB,1);
            DO(TK_MUL,1);
            DO(TK_DIV,1);
            DO(TK_NIL,3);
            DO(TK_BREAK,5);
            DO(TK_COMMA,1);
            DO(TK_IF,2);
            DO(TK_ELIF,4);
            DO(TK_ELSE,4);
            DO(TK_FOR,3);
            DO(TK_LOOP,4);
            DO(TK_RET,6);
            DO(TK_FUNC,4);
            DO(TK_ASSIGN,1);
            DO(TK_COLON,1);
            DO(TK_MOD,1);

    #undef DO
        default: assert(0); return -1;
        }
    } while(1);
    return 0;
}

static
int skip_block( struct pure* f ) {
    int recursive = 1;
    char buf[PURE_MAX_LOCAL_BUF_SIZE];
    const char* s;
    int offset;
    double num;
    int tk;

    assert(f->tk == TK_LBRA);
    tk = next_tk(f,1);

    while( recursive != 0 ) {
        switch(tk) {
        case TK_EOF:
            return -1;
        case TK_STR:
            s = str(f,&offset,buf);
            if(s == NULL)
                return -1;
            if(s != buf)
                free(cast(void*,s));
            tk = next_tk(f,offset);
            break;
        case TK_NUM:
            if((offset=number(f,&num)) <=0)
                return -1;
            tk = next_tk(f,offset);
            break;
        case TK_VAR:
            if((offset=variable(f,buf)) <=0)
                return -1;
            tk = next_tk(f,offset);
            break;
        case TK_LBRA:
            ++recursive;
            tk = next_tk(f,1);
            break;
        case TK_RBRA:
            --recursive;
            tk = next_tk(f,1);
            break;

#define DO(t,o) \
        case t: \
        tk = next_tk(f,o); \
        break

            DO(TK_EQ,2);
            DO(TK_NEQ,2);
            DO(TK_LT,1);
            DO(TK_LET,2);
            DO(TK_GT,1);
            DO(TK_GET,2);
            DO(TK_NOT,1);
            DO(TK_OR,2);
            DO(TK_AND,2);
            DO(TK_LSQR,1);
            DO(TK_RSQR,1);
            DO(TK_LPAR,1);
            DO(TK_RPAR,1);
            DO(TK_SEMICON,1);
            DO(TK_ADD,1);
            DO(TK_SUB,1);
            DO(TK_MUL,1);
            DO(TK_DIV,1);
            DO(TK_NIL,3);
            DO(TK_BREAK,5);
            DO(TK_COMMA,1);
            DO(TK_IF,2);
            DO(TK_ELIF,4);
            DO(TK_ELSE,4);
            DO(TK_FOR,3);
            DO(TK_LOOP,4);
            DO(TK_RET,6);
            DO(TK_FUNC,4);
            DO(TK_ASSIGN,1);
            DO(TK_COLON,1);
            DO(TK_MOD,1);

#undef DO
        default: return -1;
        }
    }
    return 0;
}

static
int interp( struct pure* f ) {
    assert(f->cur_result != NULL);
    set_pc(f,0); /* set the counter */
    while(f->tk != TK_EOF) {
        if( interp_global(f) != 0 )
            return -1;
    }
    return 0;
}

static
const char* pure_last_error( struct pure* p , int* ec , int* line , int* pos ) {
    size_t i;
    *ec = p->ec;
    /* calculate the line/pos number pair */
    *line=*pos=0;
    for(i = 0 ; i < p->loc && p->data[i] ; ++i ) {
        if(p->data[i]=='\n') {
            ++(*line);
            *pos=0;
        } else {
            (*pos)++;
        }
    }

    switch(p->ec) {
    case PURE_EC_NO_ERROR: return "No error";
    case PURE_EC_TOO_MANY_FUNCTION_PARAMETERS: return "Too many function parameters";
    case PURE_EC_NO_SUCH_FUNC: return "No such function";
    case PURE_EC_NO_SUCH_VALUE: return "No such value";
    case PURE_EC_FUNCTION_EXECUTION_ERROR: return p->ex_err_str;
    case PURE_EC_UNKNOWN_CONSTANT_SYMBOL: return "Cannot parse the constant value you provide";
    case PURE_EC_UNKNOWN_UNARY_OPRAND: return "Unknown unary operand";
    case PURE_EC_ONLY_NUMBER_CAN_BE_FACTOR: return "Factor can only accept number";
    case PURE_EC_ONLY_NUMBER_CAN_BE_TERM: return "Term can only accept number";
    case PURE_EC_TYPE_ERROR_FOR_COMPARTOR: return "Compare operand error";
    case PURE_EC_DIV_ZERO: return "Divide zero";
    case PURE_EC_ARRAY_EXPECT_RIGHT_SQURA: return "Expect ]";
    case PURE_EC_UNKNOWN_NUMBER: return "Unknown number";
    case PURE_EC_UNKNOWN_STRING_LITERAL: return "Unknown string literal";
    case PURE_EC_UNKNOWN_VARIABLE: return "Unknown variable";
    case PURE_EC_INVALID_FOR_LOOP: return "Invalid for loop";
    case PURE_EC_BREAK_IN_NONE_LOOP_SCOPE: return "Break in a non loop scope";
    case PURE_EC_FOR_LOOP_EXPECT_VAR: return "For loop expect a variable name as first parameter";
    case PURE_EC_SYNTAX_ERROR: return "Syntax error";
    case PURE_EC_FUNCTION_NO_BODY: return "Function has no body";
    case PURE_EC_FUNCTION_REDEFINE: return "Function redefinition";
    case PURE_EC_EXPECT_SEMICON: return "Expect ;";
    case PURE_EC_INDEX_ON_NON_ARRAY_OR_MAP: return "Index on type that is not an array or map";
    case PURE_EC_ARRAY_INDEX_SHOULD_BE_INTEGER: return "Index for array should be integer";
    case PURE_EC_MAP_INDEX_SHOULD_BE_STRING: return "Index for map should be string";
    case PURE_EC_NO_SUCH_FILE: return "No such source file";
    case PURE_EC_STACK_OVERFLOW: return "Function stack overflow";
    case PURE_EC_UNKNOWN_MAP_KEY: return "Unknown map key";
    case PURE_EC_EXPECT_COLON: return "Expect :";
    case PURE_EC_EXPECT_COMMA: return "Expect ,";
    case PURE_EC_GLOBAL_SCOPE_SYNTAX_ERROR: return "Global scope syntax error, only assignment/function call/function definition is allowed";
    case PURE_EC_UNKNOWN_LEFT_HANDSIDE_VALUE: return "Left hand side value must be variable,array/map index";
    default: return "";
    }
}

#ifndef NO_BUILT_IN_LIB
static
const char* pure_type_name( int tp ) {
    switch(tp) {
    case PURE_INVALID: return "Invalid";
    case PURE_NIL: return "Nil";
    case PURE_NUMBER: return "Number";
    case PURE_ARRAY: return "Array";
    case PURE_MAP: return "Map";
    case PURE_USER_DATA: return "UserData";
    default: return "Unknown";
    }
}
/* helper function to format the error */
#define expect_par_sz(p,exp_sz,sz,fname) \
    do { \
        if( (exp_sz) != (sz) ) { \
            sprintf((p)->ex_err_str,"The parameter size of function %s not match, exepct %d but get %d.", \
            fname,exp_sz,sz); \
            (p)->ec = PURE_EC_FUNCTION_EXECUTION_ERROR; \
            return -1; \
        } \
    } while(0)

#define expect_par_type(p,i,exp_tp,tp,fname) \
    do { \
        if( (exp_tp) != (tp) ) { \
            sprintf((p)->ex_err_str,"The parameter %d of function %s type not match, expect %s but get %s", \
            i,fname,pure_type_name(exp_tp),pure_type_name(tp)); \
            (p)->ec = PURE_EC_FUNCTION_EXECUTION_ERROR; \
            return -1; \
        } \
    } while(0)

#define func_set_err(p,reason,fname) \
    do { \
        sprintf((p)->ex_err_str,"The function %s execution has error:%s",reason); \
        (p)->ec = PURE_EC_FUNCTION_EXECUTION_ERROR; \
    } while(0);

static
const char* to_str( const char* s , char buf[PURE_MAX_LOCAL_BUF_SIZE] , size_t* len ) {
    *len = strlen(s);
    if( *len >= PURE_MAX_LOCAL_BUF_SIZE )
        return STRDUP(s);
    else
        return strcpy(buf,s);
}

/* to_str */
static 
int lib_to_str( struct pure* p , struct pure_value* par , size_t sz , 
                struct pure_value* result , void* ptr ) {
    char buf[256];
    int len;
    struct pure_shared_value* sv;

    pure_value_invalid(result);
    expect_par_sz(p,1,sz,"to_str");
    if( par[0].type == PURE_STRING ) {
        ref_val(p,result,par);
        return 0;
    }
    /* We expect a number */
    expect_par_type(p,1,PURE_NUMBER,par[0].type,"to_str");
    sv = malloc_shared_value(p);
    len = sprintf(buf,"%.6f",par->value.num);
    if( len < PURE_MAX_LOCAL_BUF_SIZE ) {
        sv->value.str.c_str=strcpy(sv->buf,buf);
    } else {
        sv->value.str.c_str = STRDUP(buf);
    }
    sv->value.str.sz = cast(size_t,len);
    pure_value_str(result,sv);
    return 0;
}
/* to_num */
static
int lib_to_num( struct pure* p , struct pure_value* par , size_t sz ,
                struct pure_value* result , void* ptr ) {
    expect_par_sz(p,1,sz,"to_num");
    if( par->type == PURE_NUMBER ) {
        ref_val(p,result,par);
        return 0;
    }
    expect_par_type(p,1,PURE_STRING,par->type,"to_num");
    errno = 0;
    result->value.num = strtod(shared_value(result)->value.str.c_str,NULL);
    if(errno !=0) {
        func_set_err(p,strerror(errno),"to_num");
        return -1;
    }
    pure_value_number(result,result->value.num);
    return 0;
}

/* is_int */
static
int lib_is_int( struct pure* p , struct pure_value* par , size_t sz ,
                struct pure_value* result , void* ptr ) {
    expect_par_sz(p,1,sz,"is_int");
    pure_value_invalid(result);
    if( par->type == PURE_NUMBER ) {
        if( is_int(par->value.num) )
            pure_value_number(result,1);
    }
    if( result->type == PURE_INVALID )
        pure_value_number(result,0);
    return 0;
}

/* to_int */
static
int lib_to_int( struct pure* p ,struct pure_value* par , size_t sz ,
                struct pure_value* result , void* ptr ) {
    expect_par_sz(p,1,sz,"to_int");
    expect_par_type(p,1,PURE_NUMBER,par->type,"to_int");
    pure_value_number(result,cast(double,to_int(par->value.num)));
    return 0;
}

/* type */
static
int lib_type( struct pure* p , struct pure_value* par , size_t sz , 
              struct pure_value* result , void* ptr ) {
    struct pure_shared_value* sv;
    expect_par_sz(p,1,sz,"type");
    sv = malloc_shared_value(p);
    switch( par->type ) {
    case PURE_INVALID:
        sv->value.str.c_str = to_str("invalid",sv->buf,&(sv->value.str.sz));
        pure_value_str(result,sv);
        return 0;
    case PURE_NUMBER:
        sv->value.str.c_str = to_str("number",sv->buf,&(sv->value.str.sz));
        pure_value_str(result,sv);
        return 0;
    case PURE_NIL:
        sv->value.str.c_str = to_str("nil",sv->buf,&(sv->value.str.sz));
        pure_value_str(result,sv);
        return 0;
    case PURE_ARRAY:
        sv->value.str.c_str = to_str("array",sv->buf,&(sv->value.str.sz));
        pure_value_str(result,sv);
        return 0;
    case PURE_MAP:
        sv->value.str.c_str = to_str("map",sv->buf,&(sv->value.str.sz));
        pure_value_str(result,sv);
        return 0;
    case PURE_USER_DATA:
        sv->value.str.c_str = to_str("userdata",sv->buf,&(sv->value.str.sz));
        pure_value_str(result,sv);
        return 0;
    default:
        sv->value.str.c_str = to_str("unknown",sv->buf,&(sv->value.str.sz));
        pure_value_str(result,sv);
        return 0;
    }
}

static
int lib_size( struct pure* p , struct pure_value* par , size_t sz ,
              struct pure_value* result, void* ptr ) {

    expect_par_sz(p,1,sz,"size");
    switch(par->type) {
    case PURE_ARRAY:
        pure_value_number(result,shared_value(par)->value.arr.sz);
        return 0;
    case PURE_MAP:
        pure_value_number(result,shared_value(par)->value.map.sz);
        return 0;
    case PURE_NUMBER:
    case PURE_NIL:
    case PURE_USER_DATA:
        pure_value_number(result,1);
        return 0;
    case PURE_INVALID:
        func_set_err(p,"Invalid parameter type","size");
        return -1;
    default: 
        func_set_err(p,"Unknown type,internal error","size");
        return -1;
    }
}

/* concate */
static
int lib_concate( struct pure* p , struct pure_value* par , size_t sz,
                 struct pure_value* result, void* ptr ) {
    char num_buf[256];
    int num_buf_sz;
    size_t i;
    char stk_buf[1024];
    char* buf = stk_buf;
    char* pos = buf;
    char* end = buf+1024;
    struct pure_shared_value* sv;

#define GROW_BUF(L) do { \
        char* tmp = malloc(L); \
        if(buf != stk_buf) free(buf); \
        memcpy(buf,tmp,end-buf); \
        end = tmp + (L); \
        pos = tmp + (pos-buf); \
        buf = tmp; \
    } while(0)

    for( i = 0 ; i < sz ; ++i ) {
        switch(par[i].type) {
        case PURE_NUMBER:
            num_buf_sz = sprintf(num_buf,"%.6f",par[i].value.num);
            if( end-pos <= num_buf_sz ) {
                GROW_BUF((end-buf)*2+num_buf_sz);
            }
            memcpy(pos,num_buf,num_buf_sz);
            pos += num_buf_sz;
            break;
        case PURE_STRING:
            if( cast(size_t,end-pos) <= shared_value(par+i)->value.str.sz ) {
                GROW_BUF((end-buf)*2+(shared_value(par+i)->value.str.sz));
            }
			memcpy(pos,shared_value(par+i)->value.str.c_str,
                shared_value(par+i)->value.str.sz);
            pos += shared_value(par+i)->value.str.sz;
            break;
        default:
            break; /* skip unknown type */
        }
    }

#undef GROW_BUF

    *pos = 0;
    sv = malloc_shared_value(p);

    if( pos-buf <= PURE_MAX_LOCAL_BUF_SIZE ) {
        sv->value.str.c_str=strcpy(sv->buf,buf);
    } else {
        if(buf == stk_buf)
            sv->value.str.c_str = STRDUP(buf);
        else
            sv->value.str.c_str = buf;
    }

    pure_value_str(result,sv);
    return 0;
}

/* arr_add */
static
int lib_arr_add( struct pure* p , struct pure_value* par , size_t sz ,
             struct pure_value* result , void* ptr ) {
    pure_value_invalid(result);
    expect_par_sz(p,2,sz,"arr_add");
    expect_par_type(p,1,PURE_ARRAY,par->type,"arr_add");
    pure_array_push_back(p,&(shared_value(par)->value.arr),par+1);
    return 0;
}

/* arr_del */
static
int lib_arr_del( struct pure* p , struct pure_value* par , size_t sz , 
                 struct pure_value* result , void* ptr ) {
    pure_value_invalid(result);
    /* We cannot remove an element in the array, so just create new one */
    expect_par_sz(p,2,sz,"arr_del");
    expect_par_type(p,1,PURE_ARRAY,par->type,"arr_del");
    expect_par_type(p,2,PURE_NUMBER,par[1].type,"arr_del");
    /* modify the original par parameters _UNDER THE HOOD_ */
    pure_array_remove(p,&(shared_value(par)->value.arr),cast(int,to_int(par[1].value.num)));
    return 0;
}

/* map_insert */
static
int lib_map_insert( struct pure* p , struct pure_value* par , size_t sz , 
                    struct pure_value* result , void* ptr ) {
    int insert;
    struct pure_value* slot;
    expect_par_sz(p,3,sz,"map_insert");
    expect_par_type(p,1,PURE_MAP,par->type,"map_insert");
    expect_par_type(p,2,PURE_STRING,par->type,"map_insert");
    /* insert new key value into the map */
    slot = symbol_table_insert(&(shared_value(par)->value.map),
        shared_value(par+1)->value.str.c_str,&insert);
    if( insert ) {
        pure_value_invalid(slot);
    }
    ref_val(p,slot,par+2);
    return 0;
}

/* print */
#ifndef NO_PRINT
static
int lib_print( struct pure* p , struct pure_value* par , size_t sz , 
               struct pure_value* result , void* ptr ) {
    size_t i;
    pure_value_invalid(result);

    for( i = 0 ; i < sz ; ++i ) {
        switch(par[i].type) {
        case PURE_NUMBER:
            printf("%.6f",par[i].value.num);
            break;
        case PURE_STRING:
            printf("%s",shared_value(par+i)->value.str.c_str);
            break;
        case PURE_NIL:
            printf("<nil>");
            break;
        case PURE_MAP:
            printf("<map>");
            break;
        case PURE_ARRAY:
            printf("<array>");
            break;
        case PURE_USER_DATA:
            printf("<user_data>");
            break;
        default:
            break;
        }
    }
    printf("\n");
    return 0;
}

#endif /*NO_PRINT*/


static
void lib_open( struct pure* p ) {
    pure_reg_func(p,"to_str",lib_to_str,NULL);
    pure_reg_func(p,"to_num",lib_to_num,NULL);
    pure_reg_func(p,"to_int",lib_to_int,NULL);
    pure_reg_func(p,"is_int",lib_is_int,NULL);
    pure_reg_func(p,"type",lib_type,NULL);
    pure_reg_func(p,"concate",lib_concate,NULL);
    pure_reg_func(p,"arr_add",lib_arr_add,NULL);
    pure_reg_func(p,"arr_del",lib_arr_del,NULL);
    pure_reg_func(p,"map_insert",lib_map_insert,NULL);
    pure_reg_func(p,"print",lib_print,NULL);
    pure_reg_func(p,"size",lib_size,NULL);

}
#endif /* NO_BUILT_IN_LIB */


/* ===========================================
 * PUBLIC INTERFACE 
 * ==========================================*/

struct pure* pure_create() {
    struct pure* f = malloc(sizeof(*f));
    symbol_table_create(&(f->c_func),PURE_MAX_CALLBACK,sizeof(struct pure_c_func));
    symbol_table_create(&(f->ctx_var),PURE_MAX_CTX_VAR,sizeof(struct pure_value));
    f->tk = TK_EOF;
    f->data= NULL;
    f->loc = 0;
    f->ec = PURE_EC_NO_ERROR;
    f->cur_result = NULL;
    f->ex_err_str[0]=0;
#ifndef NO_BUILT_IN_LIB
    lib_open(f);
#endif /* NO_BUILT_IN_LIB */
    return f;
}

void pure_delete( struct pure* f ) {
    symbol_table_delete(&(f->c_func));

    if(f->cur_result)
        pure_result_delete(f->cur_result);

    free(f);
}

void pure_reg_func( struct pure* p , const char* name , pure_cb cb , void* data ) {
    int insert;
    struct pure_c_func* func;
    func = symbol_table_insert(&(p->c_func),name,&insert);
    func->cb = cb;
    func->data = data;
}

void pure_reg_var( struct pure* p , const char* name , struct pure_value* val ) {
    int insert;
    struct pure_value* slot;
    slot = symbol_table_insert(&(p->ctx_var),name,&insert);
    if( insert ) {
        *slot = *val;
    } else {
        ref_val(p,slot,val);
        unref_val(p,val);
    }
}

void pure_print_error( struct pure* p, const char* fmt , ... ) {
    va_list vl;
    int sz;
    va_start(vl,fmt);
    sz = vsprintf(p->ex_err_str,fmt,vl);
    assert( sz < 1024 );
    p->ec = PURE_EC_FUNCTION_EXECUTION_ERROR;
}

int pure_run_file( struct pure* p , const char* file , const char** error , int* ec , int* line , int* pos  ) {
    FILE* f;
    size_t sz;
    int len;
    int ret;
    f = fopen(file,"r");

    if( f == NULL ) {
        p->ec = PURE_EC_NO_SUCH_FILE;
        return -1;
    }

    /* get the size of the file */
    fseek(f,0,SEEK_END);
    sz = cast(size_t,ftell(f));
    fseek(f,0,SEEK_SET);
    p->data = malloc(sz+1);
    len = fread(cast(void*,p->data),1,sz,f);
    assert( cast(size_t,len) < sz+1 );
    cast(char*,p->data)[len] = 0;
    fclose(f);

    if( p->cur_result ) {
        pure_result_delete(p->cur_result);
    }
    p->cur_result = pure_result_create();

    /* start to interpreting the code */
    if( interp(p) != 0 ) {
        *error = pure_last_error(p,ec,line,pos);
        pure_result_delete(p->cur_result);
        p->cur_result = NULL;
        ret = -1;
    } else {
        ret = 0;
    }

    free(cast(void*,p->data));
    p->data= NULL;
    return ret;
}

int pure_run_str( struct pure* p , const char* str , const char** error , int* ec , int* line , int* pos  ) {
    int ret;
 
    p->data = str;
    if( p->cur_result )
        pure_result_delete(p->cur_result);
    p->cur_result = pure_result_create();

    if( interp(p) != 0 ) {
        *error = pure_last_error(p,ec,line,pos);
        pure_result_delete(p->cur_result);
        ret = -1;
        p->cur_result = NULL;
    } else {
        ret = 0;
    }

    p->data = NULL;
    return ret;
}

int pure_get( struct pure* p , const char* name , struct pure_value** val ) {
    struct pure_value* ret;

    if( p->cur_result == NULL )
        return -1;

    ret = symbol_table_query(&(p->cur_result->global_var),name);
    if( ret == NULL ) {
        return -1;
    } else {
        *val = ret;
        return 0;
    }
}

int pure_iter_start( struct pure* p , const char** key , struct pure_value** val ) {
    assert(p->cur_result);
    return symbol_table_iter_start(&(p->cur_result->global_var),cast(void**,val),key);
}

int pure_iter_has_next( struct pure*  p ,int cursor ) {
    assert(p->cur_result);
    return symbol_table_iter_has_next(&(p->cur_result->global_var),cursor);
}

int pure_iter_deref( struct pure* p , int cursor , const char** key , struct pure_value** val ) {
    assert(p->cur_result);
    return symbol_table_iter_deref(&(p->cur_result->global_var),cursor,cast(void**,val),key);
}

void pure_value_new_num( struct pure* p , double n , struct pure_value* val ) {
    pure_value_invalid(val);
    if( p->cur_result == NULL )
        return;
    pure_value_number(val,n);
}

void pure_value_new_nil( struct pure* p , struct pure_value* val ) {
    pure_value_invalid(val);
    if( p->cur_result == NULL )
        return;
    pure_value_nil(val);
}

void pure_value_new_str( struct pure* p , const char* str , int own , struct pure_value* val ) {
    struct pure_shared_value* sv;
    pure_value_invalid(val);
    if( p->cur_result == NULL )
        return;
    sv = malloc_shared_value(p);
    sv->value.str.sz = strlen(str);
    if( sv->value.str.sz >= PURE_MAX_LOCAL_BUF_SIZE ) {
        sv->value.str.c_str = STRDUP(str);
    } else {
        sv->value.str.c_str = strcpy(sv->buf,str);
    }
    pure_value_str(val,sv);
}

void pure_value_new_user_data( struct pure* p , void* udata , pure_user_data_clean_cb cb , struct pure_value* val ) {
    struct pure_shared_value* sv;
    pure_value_invalid(val);
    if( p->cur_result == NULL )
        return;
    sv = malloc_shared_value(p);

    sv->value.user_data.clean_cb = cb;
    sv->value.user_data.data = udata;
    pure_value_user_data(val,sv);
}

struct pure_array* pure_value_new_arr( struct pure* p , size_t cap , struct pure_value* val ) {
    struct pure_shared_value* sv;
    pure_value_invalid(val);
    if( p->cur_result == NULL )
        return NULL;
    sv = malloc_shared_value(p);

    pure_array_create(&(sv->value.arr),cap);
    pure_value_array(val,sv);
    return &(sv->value.arr);
}

struct pure_map* pure_value_new_map( struct pure* p , size_t cap , struct pure_value* val ) {
    struct pure_shared_value* sv;
    pure_value_invalid(val);
    if( p->cur_result == NULL )
        return NULL;
    sv = malloc_shared_value(p);

    symbol_table_create(&(sv->value.map),cap,sizeof(struct pure_value));
    pure_value_map(val,sv);
    return cast(struct pure_map*,&(sv->value.map));
}

void pure_value_ref( struct pure* p , struct pure_value* lhs , const struct pure_value* rhs ) {
    ref_val(p,lhs,rhs);
}

void pure_value_unref( struct pure* p , struct pure_value* v ) {
    unref_val(p,v);
}

int pure_value_get_str( const struct pure_value* f , const char** str ) {
    if(f->type != PURE_STRING)
        return -1;
    *str = shared_value(f)->value.str.c_str;
    return 0;
}

int pure_value_get_arr( const struct pure_value* f , struct pure_array** arr ) {
    if( f->type != PURE_ARRAY )
        return -1;
    *arr = &(shared_value(f)->value.arr);
    return 0;
}

int pure_value_get_map( const struct pure_value* f , struct pure_map** map ) {
    if( f->type != PURE_MAP )
        return -1;
    *map = cast( struct pure_map* , &(shared_value(f)->value.map) );
    return 0;
}

int pure_value_get_user_data( const struct pure_value* f , void** udata ) {
    if( f->type != PURE_USER_DATA )
        return -1;
    *udata = shared_value(f)->value.user_data.data;
    return 0;
}

size_t pure_array_size( const struct pure_array* arr ) {
    return arr->sz;
}

void pure_array_push( struct pure* p , struct pure_array* arr , const struct pure_value* val ) {
    pure_array_push_back(p,arr,val);
}

int pure_array_index( const struct pure_array* arr , size_t index , struct pure_value** val ) {
    if( index >= 0 && cast(size_t,index) < arr->sz ) {
        *val = arr->arr + index;
        return 0;
    } else {
        return -1;
    }
}

int pure_map_index( const struct pure_map* map , const char* key , struct pure_value** val ) {
    struct pure_value* ret;
    ret = symbol_table_query(cast(struct symbol_table*,map),key);
    if( ret == NULL || ret->type == PURE_NIL ) {
        return -1;
    }
    *val = ret;
    return 0;
}

int pure_map_insert( struct pure* p , const struct pure_map* map , const char* key , struct pure_value* val ) {
    int insert;
    struct pure_value* ret = symbol_table_insert(cast(struct symbol_table*,map),key,&insert);
    if( insert ) {
        pure_value_invalid(ret);
    }
    ref_val(p,ret,val);
    return insert;
}

int pure_map_iter_start( const struct pure_map* map , const char** key , struct pure_value** val ) {
    return symbol_table_iter_start(cast(struct symbol_table*,map),cast(void**,val),key);
}

int pure_map_iter_has_next( const struct pure_map* map , int cursor ) {
    return symbol_table_iter_has_next(cast(struct symbol_table*,map),cursor);
}

int pure_map_iter_deref( const struct pure_map* map , int cursor , const char** key , struct pure_value** val ) {
    return symbol_table_iter_deref(cast(struct symbol_table*,map),cursor,cast(void**,val),key);
}

/* =====================================
 * TEST 
 * ====================================*/
#ifndef NDEBUG
#ifdef UTEST
/* 1 symbol_table test */
void test_symbol_table() {
    struct symbol_table tb;
    int insert;
    double* ptr;

    symbol_table_create(&tb,2,sizeof(double));
    assert(tb.cap == 2);
    assert(tb.mem_sz == sizeof(double));
    assert(tb.sz == 0);

    ptr = symbol_table_insert(&tb,"a",&insert);
    assert(ptr);
    assert(insert);
    *ptr = 1;

    ptr = symbol_table_insert(&tb,"b",&insert);
    assert(ptr);
    assert(insert);
    *ptr = 2;

    ptr = symbol_table_query(&tb,"a");
    assert(ptr != NULL);
    assert(*cast(double*,ptr) == 1);

    ptr = symbol_table_query(&tb,"b");
    assert(ptr != NULL);
    assert(*cast(double*,ptr) == 2);

    assert(tb.sz == 2);
    assert(tb.cap == 2);

    /* trigger rehash */
    ptr = symbol_table_insert(&tb,"c",&insert);
    assert(ptr);
    assert(insert);
    *cast(double*,ptr) = 3;

    assert(tb.cap ==4);
    assert(tb.sz == 3);

    ptr = symbol_table_query(&tb,"c");
    assert(ptr);
    assert( *cast(double*,ptr) == 3 );
}

/* 2. pure value test */
void test_pure_value() {
    {
        struct pure_value val;
        pure_value_invalid(&val);
        unref_val(NULL,&val);
    }
    {
        struct pure_value val,ref;
        pure_value_number(&val,1);
        ref_val(NULL,&ref,&val);
        assert( ref.type == PURE_NUMBER );
        assert( ref.value.num == 1 );
        unref_val(NULL,&val);
    }
    {
        struct pure* f = pure_create();
        struct pure_value val,ref;
        struct pure_shared_value* sv;

        f->cur_result = pure_result_create();
        sv = malloc_shared_value(f);

        sv->value.str.c_str = _strdup("Hello World");
        sv->value.str.sz = strlen("Hello World");
        pure_value_str(&val,sv);
        ref_val(f,&ref,&val);
        assert(sv->ref_cnt == 2);
        assert(ref.type == PURE_STRING);
        assert(ref.value.shared_val == val.value.shared_val);
        unref_val(f,&ref);
        assert(sv->ref_cnt == 1);
        unref_val(f,&val);
    }
    {
        struct pure* f = pure_create();
        struct pure_value val,ref;
        struct pure_shared_value* sv;

        f->cur_result = pure_result_create();
        sv = malloc_shared_value(f);

        pure_array_create(&(sv->value.arr),16);
        pure_value_array(&val,sv);

        ref_val(f,&ref,&val);
        assert(sv->ref_cnt == 2);
        assert(ref.type == PURE_ARRAY);
        assert(ref.value.shared_val == val.value.shared_val);
        unref_val(f,&ref);
        assert(sv->ref_cnt == 1);
        unref_val(f,&val);

    }
}

/* 3. Lexer test */
void test_lexer() {
    {
        char s[] = "1234.5+a5-e if ifc elif elif4 _if _else";
        struct pure* f = pure_create();
        double num;
        int tk;
        char ret[PURE_MAX_VARNAME];
        int offset;
        f->data = s;
        f->loc = 0;

        assert( (tk=next_tk(f,0)) == TK_NUM );
        assert( (offset=number(f,&num)) > 0 );
        assert( num == 1234.5 );
        assert( (tk=next_tk(f,offset)) == TK_ADD );
        assert( (tk=next_tk(f,1)) == TK_VAR );
        assert( (offset=variable(f,ret)) > 0 );
        assert( strcmp(ret,"a5") == 0 );
        assert( (tk=next_tk(f,offset)) == TK_SUB );
        assert( (tk=next_tk(f,1)) == TK_VAR );
        assert( (offset=variable(f,ret)) > 0 );
        assert( strcmp(ret,"e")==0 );
        assert( (tk=next_tk(f,offset)) == TK_IF );
        assert( (tk=next_tk(f,2)) == TK_VAR );
        assert( (offset=variable(f,ret)) > 0 );
        assert( strcmp(ret,"ifc") == 0 );
        assert( (tk=next_tk(f,offset)) == TK_ELIF );
        assert( (tk=next_tk(f,4)) == TK_VAR );
        assert( (offset=variable(f,ret)) > 0 );
        assert( strcmp(ret,"elif4") == 0 );
        assert( (tk=next_tk(f,offset)) == TK_VAR );
        assert( (offset = variable(f,ret)) > 0 );
        assert( strcmp(ret,"_if") == 0 );
        assert( (tk=next_tk(f,offset)) == TK_VAR );
        assert( (offset = variable(f,ret)) > 0 );
        assert( strcmp(ret,"_else") ==0 );
        assert( (tk=next_tk(f,offset)) == TK_EOF );
    }

    {
        char s[] = "a=\"AABBCC\\d\\e\\n\" loop(cond){} for, func func_";
        struct pure* f = pure_create();
        int tk;
        char ret[PURE_MAX_VARNAME];
        char* sl;
        int offset;
        f->data = s;
        f->loc = 0;

        assert((tk=next_tk(f,0)) == TK_VAR);
        assert((offset=variable(f,ret)) >0);
        assert(strcmp(ret,"a") == 0);
        assert((tk=next_tk(f,offset)) == TK_ASSIGN);
        assert((tk=next_tk(f,1)) == TK_STR);
        assert((sl=str(f,&offset,ret)) != NULL);
        assert(strcmp(sl,"AABBCCde\n") == 0);
        assert((tk=next_tk(f,offset)) == TK_LOOP);
        assert((tk=next_tk(f,4)) == TK_LPAR);
        assert((tk=next_tk(f,1)) == TK_VAR);
        assert((offset=variable(f,ret)) >0);
        assert(strcmp(ret,"cond") ==0);
        assert((tk=next_tk(f,offset)) == TK_RPAR);
        assert((tk=next_tk(f,1))== TK_LBRA);
        assert((tk=next_tk(f,1))== TK_RBRA);
        assert((tk=next_tk(f,1))== TK_FOR);
        assert((tk=next_tk(f,3))== TK_COMMA);
        assert((tk=next_tk(f,1))== TK_FUNC);
        assert((tk=next_tk(f,4))== TK_VAR);
        assert((offset=variable(f,ret)) >0);
        assert(strcmp(ret,"func_")==0);
    }

    {
        char s[] = "_asds=\"ABCDEFG\\a\\t\\b\\n\" nil break return #I don;t wanna see you \n_return";
        struct pure* f = pure_create();
        int tk;
        char ret[PURE_MAX_VARNAME];
        char* sl;
        int offset;
        f->data = s;
        f->loc = 0;
        assert( (tk=next_tk(f,0)) == TK_VAR );
        assert( (offset=variable(f,ret)) > 0 );
        assert( strcmp(ret,"_asds") == 0 );
        assert( (tk=next_tk(f,offset)) == TK_ASSIGN );
        assert( (tk=next_tk(f,1)) == TK_STR );
        assert( (sl=str(f,&offset,ret)) != NULL );
        assert( strcmp(sl,"ABCDEFGa\t\b\n") == 0 );
        assert( (tk=next_tk(f,offset)) == TK_NIL );
        assert( (tk=next_tk(f,3)) == TK_BREAK );
        assert( (tk=next_tk(f,5)) == TK_RET );
        assert( (tk=next_tk(f,6)) == TK_VAR );
        assert((offset=variable(f,ret)) >0);
        assert( strcmp(ret,"_return")==0 );
        assert( (tk=next_tk(f,offset)) == TK_EOF );
    }
    {
        char s[] = ">= > <= < == !=";
        struct pure* f = pure_create();
        int tk;
        f->data = s;
        f->loc = 0;
        assert( (tk=next_tk(f,0)) == TK_GET );
        assert( (tk=next_tk(f,2)) == TK_GT );
        assert( (tk=next_tk(f,1)) == TK_LET );
        assert( (tk=next_tk(f,2)) == TK_LT );
        assert( (tk=next_tk(f,2)) == TK_EQ );
        assert( (tk=next_tk(f,2)) == TK_NEQ );
    }
}

/* 4. Parser/Evaluator test */
void test_parser_expr() {
    {
        char str[] = "1+2*3";
        struct pure_value val;
        struct pure* p = pure_create();
        p->data = str;
        p->loc = 0;
        next_tk(p,0);

        assert(expr(p,&val) ==0);
        assert(val.type == PURE_NUMBER);
        assert(val.value.num == 1+2*3);
    }

    {
        char str[] = "(1+2)%4>3";
        struct pure_value val;
        struct pure* p = pure_create();
        p->data = str;
        p->loc = 0;
        next_tk(p,0);

        assert(expr(p,&val) ==0);
        assert(val.type == PURE_NUMBER);
        assert(val.value.num == 0);
    }

    {
        char str[] = "(1+2>=3)+1";
        struct pure_value val;
        struct pure* p = pure_create();
        p->data = str;
        p->loc = 0;
        next_tk(p,0);

        assert(expr(p,&val) ==0);
        assert(val.type == PURE_NUMBER);
        assert(val.value.num == 2);
    }

    {
        char str[] = "(1+3)*4 && !1 || 0";
        struct pure_value val;
        struct pure* p = pure_create();
        p->data = str;
        p->loc = 0;
        next_tk(p,0);

        assert(expr(p,&val) ==0);
        assert(val.type == PURE_NUMBER);
        assert(val.value.num == 0);
    }

    {
        char str[] = "(a*3)>=1 && 1 || !0";
        struct pure_value val;
        struct pure* p = pure_create();
        struct pure_value var;
        p->data = str;
        p->loc = 0;
        next_tk(p,0);

        /* insert a context variable */
        pure_value_number(&var,123);
        pure_reg_var(p,"a",&var);

        assert(expr(p,&val) ==0);
        assert(val.type == PURE_NUMBER);
        assert(val.value.num == 1);
    }

    {
        char str[] = "\"John\"==\"John\" && nil != 1";
        struct pure_value val;
        struct pure* p = pure_create();
        struct pure_value var;
        p->cur_result = pure_result_create();
        p->data = str;
        p->loc = 0;
        next_tk(p,0);

        /* insert a context variable */
        pure_value_number(&var,123);
        pure_reg_var(p,"a",&var);

        assert(expr(p,&val) ==0);
        assert(val.type == PURE_NUMBER);
        assert(val.value.num == 1);
    }

    {
        char str[] = "nil == c;";
        struct pure_value val;
        struct pure* p = pure_create();
        struct pure_value var;
        p->cur_result = pure_result_create();
        p->data = str;
        p->loc = 0;
        next_tk(p,0);

        /* insert a context variable */
        pure_value_number(&var,123);
        pure_reg_var(p,"a",&var);

        assert(expr(p,&val) ==0);
        assert(val.type == PURE_NUMBER);
        assert(val.value.num == 1);
    }
}

int dump_cb( struct pure* p , struct pure_value* par , size_t sz , struct pure_value* result , void*  data ) {
    size_t i = 0 ; 
    for( ; i < sz ; ++i ) {
        switch(par[i].type) {
        case PURE_NUMBER: 
            printf("[Number]%f\n",par[i].value.num);
            break;
        case PURE_NIL:
            printf("[Nil]\n");
            break;
        case PURE_ARRAY:
            printf("[Array(%d)]\n",shared_value(par+i)->value.arr.sz);
            break;
        case PURE_STRING:
            printf("[String:%d]%s\n",shared_value(par+i)->value.str.sz,shared_value(par+i)->value.str.c_str);
            break;
        default:
            assert(0); break;
        }
    }
    pure_value_nil(result);
    return 0;
}

void test_parser_array() {
    {
        char str[] = "arra=[[1,2,3],1,2,\"MyDream\"];dump(arra);";
        struct pure* p = pure_create();
        p->cur_result = pure_result_create();
        p->data = str;
        p->loc = 0;
        next_tk(p,0);
        pure_reg_func(p,"dump",dump_cb,NULL);
        assert( assign_or_call_fn(p) == 0 );
        assert( assign_or_call_fn(p) == 0 );
    }
}

int printf_cb( struct pure* p , 
               struct pure_value* par , size_t sz , 
               struct pure_value* result ,  void*  data ) {
    size_t i = 0 ; 
    for( ; i < sz ; ++i ) {
        printf("%f\n",par[i].type == PURE_NUMBER ? par[i].value.num : -1.0 );
    }
    pure_value_nil(result);
    return 0;
}

void test_assignment() {
    {
        char str[] = "var = 1234+12312*23;printf(var);";
        struct pure* p =  pure_create();

        p->cur_result = pure_result_create();
        p->loc=0;
        p->data=str;
        pure_reg_func(p,"printf",printf_cb,NULL);

        next_tk(p,0);

        assert( assign_or_call_fn(p) == 0 );
        assert( assign_or_call_fn(p) == 0 );
    }

}

void test_parser_for() {
    {
        char str[] = "for(i,1,10,1){ i=i+1*9*i;printf(i*i+1989); }";
        struct pure* p = pure_create();
        struct pure_value ret;
        int is_ret,is_brk;

        p->cur_result = pure_result_create();
        p->loc = 0;
        p->data = str;

        pure_reg_func(p,"printf",printf_cb,NULL);
        next_tk(p,0);

        assert( ctrl_for(p,&ret,&is_ret,&is_brk) == 0 );
        assert(is_ret == 0);
        assert(is_brk == 0);
        printf("================================\n");
    }

    {
        char str[] = "for(i,[1,2,3,4,5,6,7,8,9]){ i=i+1*9*i;printf(i*i+1989); }";
        struct pure* p = pure_create();
        struct pure_value ret;
        int is_ret,is_brk;

        p->cur_result = pure_result_create();
        p->loc = 0;
        p->data = str;
        
        pure_reg_func(p,"printf",printf_cb,NULL);
        next_tk(p,0);

        assert( ctrl_for(p,&ret,&is_ret,&is_brk) == 0 );
        assert(is_ret == 0);
        assert(is_brk == 0);
        printf("================================\n");
    }

}

void test_parser_loop() {
    {
        char str[] = "i=10;loop(i){ if( i == 5 ) { break ; } printf(i); i = i -1; }";
        struct pure* p = pure_create();
        struct pure_value ret;
        int is_ret = 0;
        int is_brk = 1;

        p->cur_result = pure_result_create();
        p->loc = 0;
        p->data = str;

        pure_reg_func(p,"printf",printf_cb,NULL);
        next_tk(p,0);
        assert( assign_or_call_fn(p) == 0 );
        assert( ctrl_loop(p,&ret,&is_ret,&is_brk) == 0 );
        assert(is_ret == 0);
        assert(is_brk == 1);
        printf("================================\n");
    }
}

void test_parser_cond() {
    {
        char str[] = "i=1;" \
            "if(i==1) {" \
            "printf(i);" \
            "i=i-1;}" \
            "elif(i==0){" \
            "printf(i);" \
            "i=2;}printf(i);";
        struct pure* p = pure_create();
        struct pure_value ret;
        int is_brk,is_ret;

        p->cur_result = pure_result_create();

        p->data = str;
        p->loc= 0;
        next_tk(p,0);
        pure_reg_func(p,"printf",printf_cb,NULL);

        assert( assign_or_call_fn(p) == 0 );
        assert( ctrl_cond(p,&ret,&is_ret,&is_brk) == 0 );
        assert( assign_or_call_fn(p) == 0 );

    }
}

void test_parser_func() {
    {
        char str[] = "func myfunc(i) {" \
            "printf(i);}for(i,[1,2,3,4,5]){ myfunc(i); }";
        struct pure* p = pure_create();
        struct pure_value ret;
        int is_brk=1,is_ret=0;

        p->cur_result = pure_result_create();
        pure_reg_func(p,"printf",printf_cb,NULL);

        p->data = str;
        p->loc= 0;
        next_tk(p,0);

        assert( def_fn(p) == 0 );
        assert( ctrl_for(p,&ret,&is_ret,&is_brk) == 0 );
    }
    {
        char str[] = "func myfunc(i) {" \
            "if( i == 10 ) return; else {" \
            "printf(i); i = i+1; myfunc(i); }}myfunc(1);";

        struct pure* p = pure_create();
        int is_brk=1,is_ret=0;

        p->cur_result = pure_result_create();
        pure_reg_func(p,"printf",printf_cb,NULL);

        p->data = str;
        p->loc= 0;
        next_tk(p,0);

        assert( def_fn(p) == 0 );
        assert( assign_or_call_fn(p) == 0 );
    }
}

void test_run_str() {
    {
        int ec,line,pos;
        const char* err;
        char str[] = \
            "a=1;" \
            "func fib(i) { if(i==0) return 0; elif(i==1) return 1; else return fib(i-1)+fib(i-2); }" \
            "b = fib(22); " \
            "printf(b);printf(a);";

        struct pure* p = pure_create();
        pure_reg_func(p,"printf",printf_cb,NULL);
        assert( pure_run_str(p,str,&err,&ec,&line,&pos) == 0 );
    }
}

void test_array_index() {
    {
        int ec,line,pos;
        const char* err;
        char str[] = \
            "a=[1,2,3,4];" \
            "dump(a[4]);dump(a[5]);";
        struct pure* p = pure_create();
        pure_reg_func(p,"dump",dump_cb,NULL);
        assert( pure_run_str(p,str,&err,&ec,&line,&pos) == 0 );
    }
}

void test_map_index() {
    {
        int ec,line,pos;
        const char* err;
        char str[] = \
            "a={\"A\":\"John\",\"B\":2};" \
            "dump(a[\"A\"]);dump(a[\"B\"]);";
        struct pure* p = pure_create();
        pure_reg_func(p,"dump",dump_cb,NULL);
        assert( pure_run_str(p,str,&err,&ec,&line,&pos) == 0 );
    }
}

void test_array_foreach() {
    {
        int ec,line,pos;
        const char* err;
        char str[] = \
            "a=[1,2,3,4];" \
            "func myfunc(a) { for(i,a) { dump(i); } }" \
            "myfunc(a);";

        struct pure* p = pure_create();
        pure_reg_func(p,"dump",dump_cb,NULL);
        assert( pure_run_str(p,str,&err,&ec,&line,&pos) == 0 );
    }
    {
        int ec,line,pos;
        const char* err;
        char str[] = \
            "a=[];" \
            "func myfunc(a) { for(i,a) { dump(i); } }" \
            "myfunc(a);";

        struct pure* p = pure_create();
        pure_reg_func(p,"dump",dump_cb,NULL);
        assert( pure_run_str(p,str,&err,&ec,&line,&pos) == 0 );
    }
}

void test_map_foreach() {
    {
        int ec,line,pos;
        const char* err;
        char str[] = \
            "a={\"A\":\"SomeBody\",\"B\":123};" \
            "func myfunc(a) { for(k:v,a) { dump(k);dump(v); } }" \
            "myfunc(a);";

        struct pure* p = pure_create();
        pure_reg_func(p,"dump",dump_cb,NULL);
        assert( pure_run_str(p,str,&err,&ec,&line,&pos) == 0 );
    }
}

void test_1() {
    {
        int ec,line,pos;
        const char* err;
        /* empty map dump */
        char str[] = \
            "a={};func myfunc(a){for(k,a){dump(k);dump(v);}}myfunc(a);";

        struct pure* p = pure_create();
        pure_reg_func(p,"dump",dump_cb,NULL);
        assert( pure_run_str(p,str,&err,&ec,&line,&pos) == 0 );
    }
    {
        int ec,line,pos;
        const char* err;
        /* empty array dump */
        char str[] = \
            "func myfunc(){ a=[]; for(k,a){dump(k);} }myfunc();";
        struct pure* p = pure_create();
        pure_reg_func(p,"dump",dump_cb,NULL);
        assert( pure_run_str(p,str,&err,&ec,&line,&pos) == 0 );
    }
    {
        int ec,line,pos;
        const char* err;
        /* empty function definition */
        char str[] = \
            "func f1(){ a = 1; return a; } func f2(i) { return 2*i; } f1(); f2(10); ";
        struct pure* p = pure_create();
        pure_reg_func(p,"dump",dump_cb,NULL);
        assert( pure_run_str(p,str,&err,&ec,&line,&pos) == 0 );
    }
    {
        int ec,line,pos;
        const char* err;
        /* empty function definition */
        char str[] = \
            "arr=[1,2,3,4];arr[3]=nil;func print_arr(arr){for(k,arr){print(k);}}print_arr(arr);print(size(arr));";
        struct pure* p = pure_create();
        pure_reg_func(p,"print",printf_cb,NULL);
        assert( pure_run_str(p,str,&err,&ec,&line,&pos) == 0 );
        err = NULL;
    }
}

void test_2() {
    
    {
        int ec,line,pos;
        const char* err;
        char str[] = "func IsOdd(num) { \
            if( num % 2 == 0 )  \
                return 0; \
            else \
                return 1; \
            } \
            func FilterOutOdd(arr) { \
                ret = []; \
                for( i , arr ) { \
                    if( IsOdd(i) ) { \
                        arr_add(ret,i); \
                    } \
                } \
                return ret; \
            } \
            arr = FilterOutOdd([1,2,3,4,5,6,7,8,9,10]); \
            func dumpArr(a) { \
                for(k,a) { \
                    print(k); \
                } \
            } \
            dumpArr(arr);";
        struct pure* p = pure_create();
        assert( pure_run_str(p,str,&err,&ec,&line,&pos) == 0 );
    }
    
    {
        int ec,line,pos;
        const char* err;
        char str[] = "SimpleMap = {\"Key1\":\"Value1\",\"Key2\":\"Value2\",\"Key3\":1234,\"Key4\":[1,2,3,4,5]}; \
            print(SimpleMap[\"Key1\"]); \
        print(SimpleMap[\"Key2\"]); \
        SimpleMap[\"Key2\"] = nil; \
        print(SimpleMap[\"Key2\"]);";
        struct pure* p = pure_create();
        assert( pure_run_str(p,str,&err,&ec,&line,&pos) == 0 );
    }
}

int main() {
    printf("-------------------------Unit test start!--------------------\n");
    test_symbol_table();
    test_pure_value();
    test_lexer();
    test_parser_expr();
    test_parser_array();
    test_assignment();
    test_parser_for();
    test_parser_loop();
    test_parser_cond();
    test_parser_func();
    test_run_str();
    test_map_index();
    test_array_foreach();
    test_map_foreach();
    test_1();
    test_2(); 
    printf("-------------------------Unit test finish!--------------------\n");
    return 0;
}
#endif /* UTEST */
#endif 

#ifdef NDEBUG
#ifdef PERF_TEST
#include <time.h>

int printf_cb( struct pure* p , 
struct pure_value* par , size_t sz , 
struct pure_value* result ,  void*  data ) {
    size_t i = 0 ; 
    for( ; i < sz ; ++i ) {
        printf("%f\n",par[i].type == PURE_NUMBER ? par[i].value.num : -1.0 );
    }
    pure_value_nil(result);
    return 0;
}


static
int fib(int i) {
    if(i==0) return 0; 
    else if(i==1) return 1; 
    else return fib(i-1)+fib(i-2);
}

void performance() {
    int ec,line,pos;
    const char* err;
    char str[] = "func fib(i) { if(i==0) return 0; elif(i==1) return 1; else return fib(i-1)+fib(i-2); } printf(fib(30));";
    struct pure* p = pure_create();
    clock_t start;
    pure_reg_func(p,"printf",printf_cb,NULL);

    start = clock();
    pure_run_str(p,str,&err,&ec,&line,&pos);
    printf("time:%d\n",clock()-start);

    start = clock();
    printf("\n%d\n",fib(30));
    printf("time:%d\n",clock()-start);
}

int main() {
    performance();
    return 0;
}

#endif /* PERF_TEST */
#endif /* NDEBUG */

 
#ifdef __cplusplus
}
#endif /*__cplusplus */
