/****************************************************************************
 *                             G N A T C O L L                              *
 *                                                                          *
 *                        Copyright (C) 2023, AdaCore                       *
 *                                                                          *
 * This library is free software;  you can redistribute it and/or modify it *
 * under terms of the  GNU General Public License  as published by the Free *
 * Software  Foundation;  either version 3,  or (at your  option) any later *
 * version. This library is distributed in the hope that it will be useful, *
 * but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- *
 * TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 ****************************************************************************/

/* Provide helper functions to implement in GNATCOLL.CPP.Strings a wrapper
   of the C++ ISO/IEC 14882:1998(E) string class. */

#include <string>
using namespace std;

struct CPP_String {
   string *str;
};

extern "C" void gnatcoll_cpp_append_string
  (CPP_String* s,
   const CPP_String* text)
{
   (s->str) -> append(*(text->str));
}

extern "C" void gnatcoll_cpp_append_substring
  (CPP_String* s,
   const CPP_String* text,
   size_t subpos,
   size_t sublen)
{
   (s->str) -> append(*(text->str), subpos, sublen);
}

extern "C" void gnatcoll_cpp_append_text
  (CPP_String* s,
   const char* text)
{
   (s->str) -> append(text);
}

extern "C" void gnatcoll_cpp_append_buffer
  (CPP_String* s,
   const char* text,
   size_t n)
{
   (s->str) -> append(text, n);
}

extern "C" void gnatcoll_cpp_append_fill
  (CPP_String* s,
   size_t n,
   char c)
{
   (s->str) -> append(n, c);
}

extern "C" void gnatcoll_cpp_assign_string
  (CPP_String* s,
   const CPP_String* text)
{
   *(s->str) = *(text->str);
}

extern "C" void gnatcoll_cpp_assign_text
  (CPP_String* s,
   const char* text)
{
   *(s->str) = text;
}

extern "C" void gnatcoll_cpp_assign_char
  (CPP_String* s,
   const char c)
{
   *(s->str) = c;
}

extern "C" size_t gnatcoll_cpp_capacity
  (CPP_String* s)
{
   return (s->str) -> capacity();
}

extern "C" char gnatcoll_cpp_char_at
  (CPP_String* s,
   size_t pos)
{
   return ((s->str) -> at(pos));
}

extern "C" int gnatcoll_cpp_compare
  (CPP_String* left,
   CPP_String* right)
{
   return (left->str) -> compare(*(right->str));
}

extern "C" int gnatcoll_cpp_compare_with_substring
  (CPP_String* left,
   size_t pos,
   size_t len,
   CPP_String *right)
{
   return (left->str) -> compare(pos, len, *(right->str));
}

extern "C" int gnatcoll_cpp_compare_substrings
  (CPP_String* left,
   size_t pos,
   size_t len,
   CPP_String* right,
   size_t subpos,
   size_t sublen)
{
   return (left->str) -> compare(pos, len, *(right->str), subpos, sublen);
}

extern "C" int gnatcoll_cpp_compare_with_text
  (CPP_String* left,
   const char* right)
{
   return (left->str) -> compare(right);
}

extern "C" int gnatcoll_cpp_compare_substring_with_text
  (CPP_String* left,
   size_t pos,
   size_t len,
   const char* right)
{
   return (left->str) -> compare(pos, len, right);
}

extern "C" int gnatcoll_cpp_compare_substring_with_buffer
  (CPP_String* left,
   size_t pos,
   size_t len,
   const char* right,
   size_t n)
{
   return (left->str) -> compare(pos, len, right, n);
}

extern "C" void gnatcoll_cpp_clear
  (CPP_String* s)
{
   (s->str) -> clear();
}

extern "C" void gnatcoll_cpp_copy
  (CPP_String* s,
   char* to_str,
   size_t len,
   size_t pos,
   size_t* num_bytes)
{
   *num_bytes = (s->str) -> copy(to_str, len, pos);
}

extern "C" const char* gnatcoll_cpp_c_str
  (CPP_String* s)
{
   return (s->str) -> c_str();
}

extern "C" const char* gnatcoll_cpp_data
  (CPP_String* s)
{
   return (s->str) -> data();
}

extern "C" void gnatcoll_cpp_destroy
  (CPP_String* s)
{
   free (s->str);
}

extern "C" bool gnatcoll_cpp_empty
  (CPP_String* s)
{
   return (s->str) -> empty();
}

extern "C" void gnatcoll_cpp_erase_sequence
  (CPP_String* s,
   size_t pos,
   size_t len)
{
   (s->str) -> erase(pos, len);
}

extern "C" size_t gnatcoll_cpp_find_first_not_of_string
  (CPP_String* s,
   const CPP_String* text,
   size_t pos)
{
   return (s->str) -> find_first_not_of(*(text->str), pos);
}

extern "C" size_t gnatcoll_cpp_find_first_not_of_text
  (CPP_String* s,
   const char* text,
   size_t pos)
{
   return (s->str) -> find_first_not_of(text, pos);
}

extern "C" size_t gnatcoll_cpp_find_first_not_of_buffer
  (CPP_String* s,
   const char* text,
   size_t pos, size_t n)
{
   return (s->str) -> find_first_not_of(text, pos, n);
}

extern "C" size_t gnatcoll_cpp_find_first_not_of_char
  (CPP_String* s,
   char c,
   size_t pos)
{
   return (s->str) -> find_first_not_of(c, pos);
}

extern "C" size_t gnatcoll_cpp_find_first_of_string
  (CPP_String* s,
   const CPP_String* text,
   size_t pos)
{
   return (s->str) -> find_first_of(*(text->str), pos);
}

extern "C" size_t gnatcoll_cpp_find_first_of_text
  (CPP_String* s,
   const char* text,
   size_t pos)
{
   return (s->str) -> find_first_of(text, pos);
}

extern "C" size_t gnatcoll_cpp_find_first_of_buffer
  (CPP_String* s,
   const char* text,
   size_t pos, size_t n)
{
   return (s->str) -> find_first_of(text, pos, n);
}

extern "C" size_t gnatcoll_cpp_find_first_of_char
  (CPP_String* s,
   char c,
   size_t pos)
{
   return (s->str) -> find_first_of(c, pos);
}

extern "C" size_t gnatcoll_cpp_find_last_not_of_string
  (CPP_String* s,
   const CPP_String* text,
   size_t pos)
{
   return (s->str) -> find_last_not_of(*(text->str), pos);
}

extern "C" size_t gnatcoll_cpp_find_last_not_of_text
  (CPP_String* s,
   const char* text,
   size_t pos)
{
   return (s->str) -> find_last_not_of(text, pos);
}

extern "C" size_t gnatcoll_cpp_find_last_not_of_buffer
  (CPP_String* s,
   const char* text,
   size_t pos,
   size_t n)
{
   return (s->str) -> find_last_not_of(text, pos, n);
}

extern "C" size_t gnatcoll_cpp_find_last_not_of_char
  (CPP_String* s,
   char c,
   size_t pos)
{
   return (s->str) -> find_last_not_of(c, pos);
}

extern "C" size_t gnatcoll_cpp_find_last_of_string
  (CPP_String* s,
   const CPP_String* text,
   size_t pos)
{
   return (s->str) -> find_last_of(*(text->str), pos);
}

extern "C" size_t gnatcoll_cpp_find_last_of_text
  (CPP_String* s,
   const char* text,
   size_t pos)
{
   return (s->str) -> find_last_of(text, pos);
}

extern "C" size_t gnatcoll_cpp_find_last_of_buffer
  (CPP_String* s,
   const char* text,
   size_t pos,
   size_t n)
{
   return (s->str) -> find_last_of(text, pos, n);
}

extern "C" size_t gnatcoll_cpp_find_last_of_char
  (CPP_String* s,
   char c,
   size_t pos)
{
   return (s->str) -> find_last_of(c, pos);
}

extern "C" size_t gnatcoll_cpp_find_string
  (CPP_String* s,
   const CPP_String* text,
   size_t pos)
{
   return (s->str) -> find(*(text->str), pos);
}

extern "C" size_t gnatcoll_cpp_find_text
  (CPP_String* s,
   const char* text,
   size_t pos)
{
   return (s->str) -> find(text, pos);
}

extern "C" size_t gnatcoll_cpp_find_buffer
  (CPP_String* s,
   const char* text,
   size_t pos,
   size_t n)
{
   return (s->str) -> find(text, pos, n);
}

extern "C" size_t gnatcoll_cpp_find_char
  (CPP_String* s,
   char c,
   size_t pos)
{
   return (s->str) -> find(c, pos);
}

extern "C" void gnatcoll_cpp_init
  (CPP_String* s)
{
   s->str = new std::string();
}

extern "C" void gnatcoll_cpp_init_with_fill
  (CPP_String* s,
   size_t n,
   char c)
{
   s->str = new std::string(n,c);
}

extern "C" void gnatcoll_cpp_init_with_text
  (CPP_String* s,
   const char* text)
{
   s->str = new std::string(text);
}

extern "C" void gnatcoll_cpp_insert_string
  (CPP_String* s,
   size_t pos,
   const CPP_String* text)
{
   (s->str) -> insert(pos, *(text->str));
}

extern "C" void gnatcoll_cpp_insert_substring
  (CPP_String* s,
   size_t pos,
   const CPP_String* text,
   size_t subpos,
   size_t sublen)
{
   (s->str) -> insert(pos, *(text->str), subpos, sublen);
}

extern "C" void gnatcoll_cpp_insert_text
  (CPP_String* s,
   size_t pos,
   const char *text)
{
   (s->str) -> insert(pos, text);
}

extern "C" void gnatcoll_cpp_insert_buffer
  (CPP_String* s,
   size_t pos,
   const char *text,
   size_t n)
{
   (s->str) -> insert(pos, text, n);
}

extern "C" void gnatcoll_cpp_insert_fill
  (CPP_String* s,
   size_t pos,
   size_t n,
   char c)
{
   (s->str) -> insert(pos, n, c);
}

extern "C" size_t gnatcoll_cpp_length
  (CPP_String* s)
{
   return (s->str) -> length();
}

extern "C" size_t gnatcoll_cpp_max_size
  (CPP_String* s)
{
   return (s->str) -> max_size();
}

extern "C" size_t gnatcoll_cpp_npos() {
   return string::npos;
}

extern "C" void gnatcoll_cpp_pop_back
  (CPP_String* s)
{
   (s->str) -> pop_back();
}

extern "C" void gnatcoll_cpp_push_back
  (CPP_String* s,
   char c)
{
   (s->str) -> push_back(c);
}

extern "C" void gnatcoll_cpp_replace_string
  (CPP_String* s,
   size_t pos,
   size_t len,
   const CPP_String* text)
{
   (s->str) -> replace(pos, len, *(text->str));
}

extern "C" void gnatcoll_cpp_replace_substring
  (CPP_String* s,
   size_t pos,
   size_t len,
   const CPP_String* text,
   size_t subpos, size_t sublen)
{
   (s->str) -> replace(pos, len, *(text->str), subpos, sublen);
}

extern "C" void gnatcoll_cpp_replace_text
  (CPP_String* s,
   size_t pos,
   size_t len,
   const char *text)
{
   (s->str) -> replace(pos, len, text);
}

extern "C" void gnatcoll_cpp_replace_buffer
  (CPP_String* s,
   size_t pos,
   size_t len,
   const char *text,
   size_t n)
{
   (s->str) -> replace(pos, len, text, n);
}

extern "C" void gnatcoll_cpp_replace_fill
  (CPP_String* s,
   size_t pos,
   size_t len,
   size_t n,
   char c)
{
   (s->str) -> replace(pos, len, n, c);
}

extern "C" void gnatcoll_cpp_reserve
  (CPP_String* s,
   size_t n)
{
   (s->str) -> reserve(n);
}

extern "C" void gnatcoll_cpp_resize
  (CPP_String* s, size_t n)
{
   (s->str) -> resize(n);
}

extern "C" void gnatcoll_cpp_resize_with_fill
  (CPP_String* s,
   size_t n,
   char c)
{
   (s->str) -> resize(n,c);
}

extern "C" size_t gnatcoll_cpp_reverse_find_string
  (CPP_String* s,
   const CPP_String* text,
   size_t pos)
{
   return (s->str) -> rfind(*(text->str), pos);
}

extern "C" size_t gnatcoll_cpp_reverse_find_text
  (CPP_String* s,
   const char* text,
   size_t pos)
{
   return (s->str) -> rfind(text, pos);
}

extern "C" size_t gnatcoll_cpp_reverse_find_buffer
  (CPP_String* s,
   const char* text,
   size_t pos,
   size_t n)
{
   return (s->str) -> rfind(text, pos, n);
}

extern "C" size_t gnatcoll_cpp_reverse_find_char
  (CPP_String* s,
   char c,
   size_t pos)
{
   return (s->str) -> rfind(c, pos);
}

extern "C" size_t gnatcoll_cpp_size
  (CPP_String* s)
{
   return (s->str) -> size();
}

extern "C" void gnatcoll_cpp_substr
  (CPP_String* result,
   CPP_String* s,
   size_t pos,
   size_t len)
{
   std::string aux = (s->str) -> substr(pos, len);
   const char* text = aux.c_str();

   gnatcoll_cpp_init_with_text(result, text);
}

extern "C" void gnatcoll_cpp_swap_strings
  (CPP_String* s1,
   CPP_String* s2)
{
   (s1->str) -> swap (*(s2->str));
}

// Relational Operators
// ********************

extern "C" bool gnatcoll_cpp_eq_strings
  (CPP_String* left,
   CPP_String* right)
{
   return *(left->str) == *(right->str);
}

extern "C" bool gnatcoll_cpp_eq_text_string
  (const char* left,
   CPP_String* right)
{
   return left == *(right->str);
}

extern "C" bool gnatcoll_cpp_eq_string_text
  (CPP_String* left,
   const char* right)
{
   return *(left->str) == right;
}

extern "C" bool gnatcoll_cpp_lt_strings
  (CPP_String* left,
   CPP_String* right)
{
   return *(left->str) < *(right->str);
}

extern "C" bool gnatcoll_cpp_lt_text_string
  (const char* left,
   CPP_String* right)
{
   return left < *(right->str);
}

extern "C" bool gnatcoll_cpp_lt_string_text
  (CPP_String* left,
   const char* right)
{
   return *(left->str) < right;
}

extern "C" bool gnatcoll_cpp_le_strings
  (CPP_String* left,
   CPP_String* right)
{
   return *(left->str) <= *(right->str);
}

extern "C" bool gnatcoll_cpp_le_text_string
  (const char* left,
   CPP_String* right)
{
   return left <= *(right->str);
}

extern "C" bool gnatcoll_cpp_le_string_text
  (CPP_String* left,
   const char* right)
{
   return *(left->str) <= right;
}

extern "C" bool gnatcoll_cpp_gt_strings
  (CPP_String* left,
   CPP_String* right)
{
   return *(left->str) > *(right->str);
}

extern "C" bool gnatcoll_cpp_gt_text_string
  (const char* left,
   CPP_String* right)
{
   return left > *(right->str);
}

extern "C" bool gnatcoll_cpp_gt_string_text
  (CPP_String* left,
   const char* right)
{
   return *(left->str) > right;
}

extern "C" bool gnatcoll_cpp_ge_strings
  (CPP_String* left,
   CPP_String* right)
{
   return *(left->str) >= *(right->str);
}

extern "C" bool gnatcoll_cpp_ge_text_string
  (const char* left,
   CPP_String* right)
{
   return left >= *(right->str);
}

extern "C" bool gnatcoll_cpp_ge_string_text
  (CPP_String* left,
   const char* right)
{
   return *(left->str) >= right;
}
