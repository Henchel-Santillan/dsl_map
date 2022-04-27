#ifndef DSL_SET_H
#define DSL_SET_H


#include "details/rb_tree.h"

#include <cstddef>
#include <functional>
#include <initializer_list>
#include <iterator>
#include <limits>
#include <memory>
#include <memory_resource>
#include <stdexcept>


namespace dsl {

    template <class Key, class Compare = std::less<Key>>
    class set {
    public:

        //*** Member Types ***//

        using key_type = Key;
        using value_type = Key;
        using size_type = std::size_t;
        using difference_type = std::ptrdiff_t;

        using key_compare = Compare;
        using value_compare = Compare;
        using allocator_type = std::pmr::polymorphic_allocator<std::byte>;

        using reference = value_type&;
        using const_reference = const value_type&;
        using pointer = std::allocator_traits<allocator_type>::pointer;
        using const_pointer = std::allocator_traits<allocator_type>::const_pointer;


        //*** Member Functions ***//

        //* Constructors *//

        explicit set(const Compare &comp, 
                     allocator_type allocator = {})
            : m_allocator(allocator)
            , m_key_comp(comp)
            , m_value_comp(comp)
        {} 

        explicit set(allocator_type allocator = {})
            : m_allocator(allocator)
            , m_key_comp(Compare())
            , m_value_comp(Compare())
        {}

        template <class InputIt>
        set(InputIt first, InputIt last, 
            const Compare &comp = Compare(),
            allocator_type allocator = {})
            : set(comp, allocator)
        { insert(first, last); }

        template <class InputIt>
        set(InputIt first, InputIt last, 
            allocator_type allocator = {})
            : set(first, last, Compare(), allocator) 
        {}

        set(std::initializer_list<value_type> init, 
            const Compare &comp, 
            allocator_type allocator = {})
            : set(init.begin(), init.end(), comp, allocator)
        {}

        set(std::initializer_list<value_type> init, 
            allocator_type allocator = {})
            : set(init, Compare(), allocator) 
        {}


        //* Copy Constructors *//

        set(const set &other, allocator_type allocator = {})
            : set(allocator)
        { operator=(other); }

        set(const set &other) 
            : set(other, std::allocator_traits<allocator_type>::select_on_container_copy_construction(other.get_allocator()))
        {}


        //* Move Constructors *//

        set(set &&other, allocator_type allocator)
            : set(allocator)
        { oeprator=(std::move(other)); }

        set(set &&other) 
            : set(other, other.get_allocator()) 
        {}


        //* Destructor *//

        ~set();


        //* Assignment Operator and Allocator Access

        set& operator=(const set&);
        set& operator=(std::initializer_list<value_type> ilist);
        set& operator=(set&&) noexcept(std::allocator_traits<allocator_type>::propagate_on_container_move_assignment::value);

        allocator_type get_allocator() const noexcept;




    private:

        //*** Members ***//

        allocator_type m_allocator;

        Compare m_key_comp;
        Compare m_value_comp;

    };
    
} // namespace dsl



#endif // DSL_SET_H