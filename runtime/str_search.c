/* vim: set sw=4 sts=4 et foldmethod=syntax : */

/*
 * Copyright (c) 2009 Alexander Færøy <ahf@0x90.dk>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include "str_search.h"
#include "mlvalues.h"

#include <stdlib.h>
#include <string.h>

/* Knuth-Morris-Pratt algorithm */
static int *kmp_table(const char *W, mlsize_t W_len)
{
    int *T = calloc(W_len, sizeof(int));
    mlsize_t p = 2, c = 0;

    T[0] = -1;

    while (p < W_len)
    {
        if (W[p - 1] == W[c])
        {
            T[p] = ++c;
            ++p;
        }
        else if (c > 0)
            c = T[c];
        else
        {
            T[p] = 0;
            ++p;
        }
    }

    return T;
}

value kmp_search(value s1, value s2)
{
    const char *S = String_val(s2);
    const char *W = String_val(s1);
    mlsize_t S_len = strlen(S);
    mlsize_t W_len = strlen(W);

    if (! S || ! W || ! S_len || ! W_len)
        return Val_bool(0);

    int *T = kmp_table(W, W_len);
    mlsize_t m = 0, i = 0;

    while (m + i < S_len)
    {
        if (W[i] == S[m + i])
        {
            ++i;

            if (W_len == i)
            {
                free(T);
                return Val_bool(1);
            }
        }
        else
        {
            m = m + i - T[i];

            if (i > 0)
                i = T[i];
        }
    }

    free(T);
    return Val_bool(0);
}
