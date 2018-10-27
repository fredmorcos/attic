#include "symtab.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define _POSIX_C_SOURCE 200809L

SymTab *
symtab_new()
{
  SymTab *tmp = malloc(sizeof(SymTab));

  tmp->size = 0;
  tmp->list = NULL;

  return tmp;
}

int
symtab_add_node(SymTab *table, int addr, char *lab)
{
  SymTabNode *node;
  int res;
  char *tmp_lab;

  tmp_lab = strdup(lab);
  res = strlen(tmp_lab);

  if (tmp_lab[res - 1] == ':')
    tmp_lab[res - 1] = '\0';

  res = symtab_get_val(table, tmp_lab);
  if (res != -1)
    return -1;

  table->size++;
  table->list = realloc(table->list, table->size * sizeof(SymTabNode *));
  node = malloc(sizeof(SymTabNode));
  node->addr = addr;
  node->lab = strdup(tmp_lab);
  table->list[table->size - 1] = node;

  return 0;
}

void
symtab_print(SymTab *table)
{
  int i = 0;
  for (i = 0; i < table->size; i++)
    printf("%s %d\n", table->list[i]->lab, table->list[i]->addr);
}

int
symtab_get_val(SymTab *table, char *lab)
{
  int i = 0;
  for (i = 0; i < table->size; i++)
    if (strcmp(lab, table->list[i]->lab) == 0)
      return table->list[i]->addr;
  return -1;
}

void
symtab_free(SymTab *table)
{
  if (table->list)
    {
      int i = 0;
      for (i = 0; i < table->size; i++)
	{
	  if (table->list[i])
	    {
	      free(table->list[i]->lab);
	      free(table->list[i]);
	    }
	}
      free(table->list);
    }

  free(table);
}
