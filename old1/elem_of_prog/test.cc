#include <iostream>

#include "bbtree.h"
#include "bbcoord.h"
using namespace eop;

void print(int i) {
	std::cout << " " << i;
}

/*
    t is a binary tree like this:

                           0

                           |
                           |
                   1-------+--------2
                   |                |
                   |                |
                   +---+       +----+---+
                       |       |        |
                       |       |        |
                       3       4        5
 */

int main() {
	bbtree<int> t;
	typedef bbtree<int>::coord bbc;
	bbc c0 = t.insert_root(0);
	bbc c1 = t.insert_left(c0, 1);
	bbc c2 = t.insert_right(c0, 2);
	t.insert_right(c1, 3);
	t.insert_left(c2, 4);
	t.insert_right(c2, 5);
	std::cout << "pre" << std::endl;
	traverse_pre(c0, print);
	std::cout << std::endl;
	std::cout << "in" << std::endl;
	traverse_in(c0, print);
	std::cout << std::endl;
	std::cout << "post" << std::endl;
	traverse_post(c0, print);
	std::cout << std::endl;
	return 0;
}
