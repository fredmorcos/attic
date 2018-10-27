#ifndef SYMTAB_H
#define SYMTAB_H

typedef struct _symtab_node
{
  int addr;
  char *lab;
} SymTabNode;

typedef struct _symtab
{
  int size;
  SymTabNode **list;
} SymTab;

SymTab *program_table,
  *data_table;

SymTab *symtab_new();
int symtab_add_node(SymTab *, int, char *);
int symtab_get_val(SymTab *, char *);
void symtab_print(SymTab *);
void symtab_free(SymTab *);

#endif
