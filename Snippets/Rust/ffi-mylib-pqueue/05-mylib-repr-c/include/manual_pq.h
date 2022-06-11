#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

struct PriorityQueueU8;

struct PriorityQueueU8 *pq_create(const uint8_t *const elements, size_t len);
bool pq_push(struct PriorityQueueU8 *const pq, const uint8_t element);
bool pq_pop(struct PriorityQueueU8 *const pq, uint8_t *const result);
void pq_free(struct PriorityQueueU8 *pq);
