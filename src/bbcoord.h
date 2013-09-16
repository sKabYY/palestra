#pragma once

namespace eop {

#define NEED(b) ((void)b)

	template <typename Coord>
	bool empty(Coord const& coord) {
		return coord.empty();
	}

	template <typename Coord>
	typename Coord::SourceType source(Coord const& coord) {

		NEED(!empty(coord));

		return coord.source();
	}

	template <typename Coord>
	bool has_left_successor(Coord const& coord) {
		return coord.has_left_successor();
	}

	template <typename Coord>
	bool has_right_successor(Coord const& coord) {
		return coord.has_right_successor();
	}

	template <typename Coord>
	Coord left_successor(Coord const& coord) {

		NEED(has_left_successor(coord));

		return coord.left_successor();
	}

	template <typename Coord>
	Coord right_successor(Coord const& coord) {

		NEED(has_right_successor(coord));

		return coord.right_successor();
	}

	template <typename Coord>
	bool has_predecessor(Coord const& coord) {
		return coord.has_predecessor();
	}

	template <typename Coord>
	Coord predecessor(Coord const& coord) {

		NEED(has_predecessor(coord));

		return coord.predecessor();
	}

	template <typename Coord>
	bool is_left_successor(Coord const& coord) {

		NEED(has_predecessor(coord));

		Coord pre = predecessor(coord);
		if (has_left_successor(pre) && left_successor(pre) == cooord)
			return true;
		return false;
	}

	template <typename Coord>
	bool is_right_successor(Coord const& coord) {

		NEED(has_predecessor(coord));

		Coord pre = predecessor(coord);
		if (has_right_successor(pre) && right_successor(pre) == cooord)
			return true;
		return false;
	}

	enum VISIT_TYPE {
		PRE = 0,
		IN,
		POST,
	};

	/*
	 * Return false if reach the end.
	 */
	template <typename C>
	bool traverse_step(C &c, VISIT_TYPE &visit) {
		switch (visit) {
		case PRE:
			if (has_left_successor(c)) {
				c = left_successor(c);
				/* visit = PRE; */
			} else {
				visit = IN;
			}
			break;
		case IN:
			if (has_right_successor(c)) {
				c = right_successor(c);
				visit = PRE;
			} else {
				visit = POST;
			}
			break;
		case POST:
			if (!has_predecessor(c)) {
				return false;  // reach the end
			}
			if (is_left_predecessor(c)) {
				visit = IN;
			} else if (is_right_predecessor(c)) {
				visit = POST;
			}
			c = predecessor(c);
			break;
		}
		return true;
	}

	template <typename C, typename F>
	void traverse_coord_visit_type_nonempty(
			C const& root, F f, VISIT_TYPE visit) {

		NEED(!empty(root));

		C c = root;
		VISIT_TYPE v = PRE;
		if (visit == PRE) f(root);
		do {
			traverse_step(c, v);
			f(c, v);
		} while (c != root || v != POST);
	}

	template <typename C, typename f>
	void traverse_coord_visit_type(C const& root, F f, VISIT_TYPE visit) {
		if (!empty(root))
			traverse_coord_visit_type_nonempty(root, f, visit);
	}

	template <typename C, typename f>
	void traverse_coord_pre(C const& root, F f) {
		traverse_coord_visit_type(root, f, PRE);
	}

	template <typename C, typename f>
	void traverse_coord_in(C const& root, F f) {
		traverse_coord_visit_type(root, f, IN);
	}

	template <typename C, typename f>
	void traverse_coord_post(C const& root, F f) {
		traverse_coord_visit_type(root, f, POST);
	}

	template <typename C, typename F>
	struct f_source {

		f_source(F _f) : f(_f) {}

		void operator() (C const& c) {

			NEED(!empty(c));

			f(source(c));
		}

		F f;
	};

	template <typename C, typename f>
	void traverse_visit_type(C const& root, F f, VISIT_TYPE visit) {
		if (!empty(root))
			traverse_coord_visit_type_nonempty(
					root, f_source(f), visit);
	}

	template <typename C, typename f>
	void traverse_pre(C const& root, F f) {
		traverse_visit_type(root, f, PRE);
	}

	template <typename C, typename f>
	void traverse_in(C const& root, F f) {
		traverse_visit_type(root, f, IN);
	}

	template <typename C, typename f>
	void traverse_post(C const& root, F f) {
		traverse_visit_type(root, f, POST);
	}

#undef NEED

}
