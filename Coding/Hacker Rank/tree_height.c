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

int get_height(struct node *root) {
  if (root == NULL || (!root->left && !root->right)) {
    return 0;
  }

  int lh = get_height(root->left);
  int rh = get_height(root->right);
  return (lh > rh ? lh : rh) + 1;
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

  printf("%d", get_height(root));
  return 0;
}
