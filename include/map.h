#ifndef DS_MAP_MAP_H
#define DS_MAP_MAP_H


#include <algorithm>
#include <functional>
#include <memory>
#include <memory_resource>

#include "internal/map_iterator.h"
#include "internal/policy.h"


#include "internal/entry.h"


namespace dsl::map
{
    template <class Key,
              class V,
              class Hash = std::hash<Key>,
              class KeyEqual = std::equal_to<Key>,
              class Allocator = std::polymorphic_allocator<std::byte>,
              class GrowthPolicy = internal::growth_policy
              >
    class map
    {
    public:
        //*** Member Types ***//
        using key_type = Key;
        using mapped_type = V;

        using size_type = std::size_t;
        using difference_type = std::ptrdiff_t;

        using hasher = Hash;
        using key_equal = KeyEqual;
        using allocator_type = Allocator;

        using value_type = entry_type::pair_t;
        using reference = value_type&;
        using const_reference = const value_type&;
        using pointer = std::allocator_traits<Allocator>::pointer;
        using const_pointer = std::allocator_traits<Allocator>::const_pointer;

        using iterator = internal::iterators::map_iterator<Key, V, false, false>;
        using const_iterator = internal::iterators::map_iterator<Key, V, true, false>;
        using local_iterator = internal::iterators::map_iterator<Key, V, false, true>;
        using const_local_iterator = internal::iterators::map_iterator<Key, V, true, true>;


    private:
        //*** Private Using Directives ***//
        using entry_type = details::entry<Key, V>;
        using GrowthPolicy::compute_index;
        using GrowthPolicy::compute_closest_capacity;
        using GrowthPolicy::minimum_capacity;

    };  // class map

}   // namespace dsl::map



#endif //DS_MAP_MAP_H
