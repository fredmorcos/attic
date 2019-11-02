#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct node {
  int data;
  struct node *left;
  struct node *right;
};

struct node *insert(struct node *root, int data) {
  if (root == NULL) {
    struct node *node = (struct node *)malloc(sizeof(struct node));

    node->data = data;

    node->left = NULL;
    node->right = NULL;
    return node;
  } else {
    struct node *cur;

    if (data <= root->data) {
      cur = insert(root->left, data);
      root->left = cur;
    } else {
      cur = insert(root->right, data);
      root->right = cur;
    }

    return root;
  }
}

void tree_free(struct node *root) {
  if (root->left) {
    tree_free(root->left);
  }
  if (root->right) {
    tree_free(root->right);
  }
  free(root);
}

void level_order(struct node *root) {
  struct node *q[500];
  int ql = 0;
  int e = 0;

  q[ql++] = root;

  while (e != ql) {
    if (q[e]->left) {
      q[ql++] = q[e]->left;
    }

    if (q[e]->right) {
      q[ql++] = q[e]->right;
    }

    printf("%d ", q[e++]->data);
  }
}

int main() {
  struct node *root = NULL;

  int t;
  int data;

  scanf("%d", &t);

  while (t-- > 0) {
    scanf("%d", &data);
    root = insert(root, data);
  }

  level_order(root);
  tree_free(root);
  return 0;
}
