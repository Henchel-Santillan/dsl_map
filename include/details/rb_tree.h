#ifndef DSL_RB_TREE_H
#define DSL_RB_TREE_H


#include <cstddef>
#include <iterator>

#include <memory>
#include <memory_resource>


namespace dsl::details_set {

    enum class rb_color { black = 0, red };

    /**
     * @brief Representation of a red-black tree node. 
     * Contains a color identifier (red or black) as well as pointers to children (left and right) and parent
     * 
     * @tparam Key 
     */
    template <class Key>
    struct rb_node {

        rb_node() 
            : m_key() 
            , m_color(rb_color::black)
        {}

        Key m_key;
        rb_color m_color;

        rb_node<Key> *m_left;
        rb_node<Key> *m_right;
        rb_node<Key> *m_parent;
    };

    template <class Key, class Compare> class rb_tree;

    /**
     * @brief Iterator class for a red-black tree with const pointer and reference member types.
     * Adheres to the named requirements for LegacyBidirectionalIterator.
     * 
     * @tparam Key 
     */
    template <class Key>
    class rb_tree_const_iterator {
    public:

        //*** Member Types ***//

        using iterator_category = std::bidirectional_iterator_tag;
        using difference_type = std::ptrdiff_t;
        
        using value_type = Key;
        using pointer = const value_type*;
        using reference = const value_type&;

        using node_t = rb_node<Tp>;


        //*** Member Functions ***//

        [[nodiscard]] constexpr pointer operator->() const noexcept {
            return std::addressof(m_ptr->m_key);
        }

        [[nodiscard]] constexpr reference operator*() const noexcept {
            return m_ptr->m_key;
        }

        constexpr rb_tree_const_iterator& operator++() noexcept {
            return *this;
        }

        constexpr rb_tree_const_iterator operator++(int) noexcept {
            rb_tree_const_iterator it(*this);
            ++(*this);
            return it;
        }

        constexpr rb_tree_const_iterator& operator--() noexcept {
            return *this;
        }

        constexpr rb_tree_const_iterator operator--(int) noexcept {
            rb_tree_const_iterator it(*this);
            --(*this);
            return it;
        }

        [[nodiscard]] constexpr auto operator<=>(const rb_tree_const_iterator&) const noexcept = default;


    protected:

        //*** Members ***//

        node_t *m_ptr;
        constexpr explicit rb_tree_const_iterator(const rb_node<Key> *ptr) 
            : m_ptr(const_cast<node_t*>(ptr)) {}


        //*** Friend decl ***//

        template <class Compare>
        friend class rb_tree<Key, Compare>;
    };


    /**
     * @brief Iterator class for a red-black tree with non-const pointer and reference member types. 
     * Adheres to the named requirements for LegacyBidirectionalIterator.
     * 
     * @tparam Key 
     */
    template <class Key>
    class rb_tree_iterator : public rb_tree_const_iterator<Key> {
    public:

        //*** Member Types ***//

        using base_t = rb_tree_const_iterator<Key>;
        using node_t = typename base_t::node_t;
        using value_type = typename base_t::value_type;

        using pointer = value_type*;
        using reference = value_type&;


        //*** Member Functions ***//

        [[nodiscard]] constexpr pointer operator->() const noexcept {
            return std::addressof(this->m_ptr->m_key);
        }

        [[nodiscard]] constexpr reference operator*() const noexcept {
            return this->m_ptr->m_key;
        }

        constexpr rb_tree_iterator& operator++() noexcept {
            base_t::operator++();
            return *this;
        }

        constexpr rb_tree_iterator operator++(int) noexcept {
            rb_tree_iterator it(*this);
            ++(*this);
            return it;
        }

        constexpr rb_tree_iterator& operator--() noexcept {
            base_t::operator--();
            return *this;
        }

        constexpr rb_tree_iterator operator--(int) noexcept {
            rb_tree_iterator it(*this);
            --(*this);
            return it;
        }


    private:    

        //*** Members ***//

        constexpr explicit rb_tree_iterator(const node_t *ptr)
            : rb_tree_const_iterator(ptr) {}


        //*** Friend decl ***//

        template <class Compare>
        friend class rb_tree<Key, Compare>;
    };


    template <class Key, class Compare> 
    class rb_tree {
    public:

        //*** Member Types ***//

        using value_type = Key;
        using size_type = std::size_t;
        using difference_type = std::ptrdiff_t;
        
        using compare = Compare;
        using allocator_type = std::pmr::polymorphic_allocator<std::byte>;

        using reference = value_type&;
        using const_reference = const value_type&;
        using pointer = std::allocator_traits<allocator_type>::pointer;
        using const_pointer = std::allocator_traits<allocator_type>::const_pointer;

        using iterator = rb_tree_iterator<Key>;
        using const_iterator = rb_tree_const_iterator<Key>;
        using reverse_iterator = std::reverse_iterator<iterator>;
        using const_reverse_iterator = std::reverse_iterator<const_iterator>;


        //*** Member Functions **//


    protected:

        //*** Using Directives ***//

        using node_type = rb_node<Key>;


        //*** Members ***//

        allocator_type m_allocator; 
        node_type *m_leftmost;
        node_type *m_rightmost;
    };

}   // namespace dsl::details_set

#endif // DSL_RB_TREE_H
