#pragma

namespace eop {

#ifndef NULL
#define NULL ((void*)0)
#endif

	template <typename T>
	class bbtree {

	private:
		/*
		 * struct bbtree_node
		 */
		struct bbtree_node {
			T const value;
			bbtree_node* left;
			bbtree_node* right;
			bbtree_node* parent;
			bbtree_node(T const& v, bbtree_node* p=NULL)
				: value(v), parent(p) {}
			~bbtree_node() {
				delete left;
				delete right;
			}
			bbtree_node* insert_left(T const& v) {
				left = new bbtree_node(v, this);
				return left;
			}
			bbtree_node* insert_right(T const& v) {
				right = new bbtree_node(v, this);
				return right;
			}
		};

	public:
		/*
		 * class coord
		 */
		class coord {
		public:
			typedef T SourceType;
		public:
			coord(bbtree_node* n) : node(n) {}
			~coord() {}

			coord(coord const& other) : node(other.node) {}
			coord& operator= (coord const& other) {
				node = other.node;
			}

			bool operator== (coord const& other) {
				return node == other.node;
			}

			bool operator!= (coord const& other) {
				return !(*this == other);
			}

			bool empty() const {
				return node == NULL;
			}

			T source() const {
				return node->value;
			}

			bool has_left_successor() const {
				return node->left != NULL;
			}

			bool has_right_successor() const {
				return node->right != NULL;
			}

			coord left_successor() const {
				return coord(node->left);
			}

			coord right_successor() const {
				return coord(node->right);
			}

			bool has_predecessor() const {
				return node->parent != NULL;
			}

			coord predecessor() const {
				return coord(node->parent);
			}

		private:
			friend class bbtree;
			bbtree_node* node;
		};

	public:
		bbtree() : root(NULL) {}
		~bbtree() {delete root;}

		bool empty() const {return root == NULL;}

		coord begin() const {
			return coord(root);
		}

		coord end() const {
			return coord(NULL);
		}

		coord insert_root(T const& value) {
			delete root;
			root = new bbtree_node(value);
			return coord(root);
		}

		coord insert_left(coord parent, T const& value) {
			return coord(parent.node->insert_left(value));
		}

		coord insert_right(coord parent, T const& value) {
			return coord(parent.node->insert_right(value));
		}

	private:
		bbtree(bbtree const&);
		bbtree& operator= (bbtree const&);

	private:
		bbtree_node* root;
	};

}
