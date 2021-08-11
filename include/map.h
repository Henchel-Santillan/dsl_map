#ifndef DS_MAP_MAP_H
#define DS_MAP_MAP_H


#include <algorithm>
#include <functional>
#include <memory>
#include <memory_resource>
#include <stdexcept>

#include "internal/map_iterator.h"
#include "internal/policy.h"
#include "internal/entry.h"
#include "internal/type_attr.h"


namespace dsl::map
{
    namespace details
    {
        static constexpr const float default_max_load_factor = 1.0f;

        /*template <class Hash>
        using transparent_key_equal = typename Hash::transparent_key_equal;


        // Predicate as used here is similar to map's KeyEqual template parameter
        template <class Hash, class Predicate>
        constexpr bool is_transparent_key_equal_v = is_detected<typename Predicate::is_transparent, Hash>::value;

        template <class Hash, class Predicate, class Key, bool = is_transparent_key_equal_v<Hash, Predicate>>
        struct key_equal
        { using type = Predicate; };  // struct key_equal


        // true-type struct redefinition of the above key_equal
        template <class Hash, class Predicate, class Key>
        struct key_equal<Hash, Predicate, Key, true>
        { using type = typename Hash::transparent_key_equal; };  // struct key_equal*/

    }   // namespace details

    template <class Key,
              class V,
              class Hash = std::hash<Key>,
              class KeyEqual = std::equal_to<Key>,
              class Allocator = std::polymorphic_allocator<std::byte>,
              class GrowthPolicy = internal::growth_policy>
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

        using value_type = details::entry<Key,V>::pair_t;
        using reference = value_type&;
        using const_reference = const value_type&;
        using pointer = std::allocator_traits<Allocator>::pointer;
        using const_pointer = std::allocator_traits<Allocator>::const_pointer;

        using iterator = iterators::map_iterator<Key, V, false, false>;
        using const_iterator = iterators::map_iterator<Key, V, true, false>;
        using local_iterator = iterators::map_iterator<Key, V, false, true>;
        using const_local_iterator = iterators::map_iterator<Key, V, true, true>;


        //*** Member Functions ***//
        map() noexcept
            : map(minimum_capacity()) {}

        explicit map(const size_type bucket_count,
                     const Hash& hash = Hash(),
                     const key_equal &equal = key_equal(),
                     allocator_type allocator = {})
            : m_bucket_count(bucket_count),
              m_size(0),
              m_hash(hash),
              m_key_equal(equal),
              m_max_load_factor(max_load_factor),
              m_allocator(allocator),
              m_entries(m_bucket_count <= 0 ? nullptr : std::make_unique<entry_type>(m_bucket_count))
        {
        }


        // Copy Constructor
        map(const map &rhs,
            allocator_type allocator = {})
            : map() {}


        ~map();
        constexpr void swap(const map&) noexcept;


        //*** Access and Iterators ***//
        [[nodiscard]] constexpr size_type size() const noexcept             { return m_size; }
        [[nodiscard]] constexpr size_type max_size() const noexcept         { return std::numeric_limits<size_type>::max(); }
        [[nodiscard]] constexpr size_type bucket_count() const noexcept     { return m_bucket_count; }
        [[nodiscard]] constexpr size_type max_bucket_count() const noexcept { return std::numeric_limits<size_type>::max(); }
        [[nodiscard]] constexpr float load_factor() const noexcept          { return static_cast<float>(m_size) / m_bucket_count; }
        [[nodiscard]] constexpr float max_load_factor() const noexcept      { return m_max_load_factor; }
        constexpr void max_load_factor(float ml)                            { m_max_load_factor = ml }

        [[nodiscard]] allocator_type get_allocator() const noexcept         { return m_allocator; }

        size_type bucket_size(size_type n) const;
        size_type bucket(const Key&) const;

        constexpr iterator begin() noexcept                 {  }
        constexpr const_iterator begin() const noexcept     {  }
        constexpr const_iterator cbegin() const noexcept    {  }

        constexpr iterator end() noexcept                   {  }
        constexpr const_iterator end() const noexcept       {  }
        constexpr const_iterator cend() const noexcept      {  }

        constexpr local_iterator lbegin() noexcept          {  }
        constexpr const_local_iterator lbegin() const noexcept {}
        constexpr const_local_iterator clbegin() const noexcept {}

        constexpr local_iterator lend() noexcept          {  }
        constexpr const_local_iterator lend() const noexcept {}
        constexpr const_local_iterator clend() const noexcept {}


        constexpr V& at(const Key&);
        constexpr const V& at(const Key&) const;
        constexpr V& operator[](const Key&);
        constexpr const V& operator[](const Key&) const;

        constexpr size_type count(const Key&) const;
        constexpr iterator find(const Key&);
        constexpr const_iterator find(const Key&) const;
        constexpr bool contains(const Key&) const;

        std::pair<iterator, iterator> equal_range(const Key&);
        std::pair<const_iterator, const_iterator> equal_range(const Key&) const;

        hasher hash_function() const;
        key_equal key_eq() const;


        //*** Modifiers ***//
        // See slinked_list.h for a guide on the functions to implement
        constexpr void rehash(size_type bucket_count);


    private:
        //*** Private Using Directives ***//
        using default_max_load_factor = typename details::default_max_load_factor;
        using entry_type = details::entry<Key, V>;
        using GrowthPolicy::compute_index;
        using GrowthPolicy::compute_closest_capacity;
        using GrowthPolicy::minimum_capacity;


        size_type                     m_bucket_count = 0;
        size_type                     m_size = 0;
        key_equal                     m_key_equal;
        Hash                          m_hash;
        float                         m_max_load_factor = default_max_load_factor;
        allocator_type                m_allocator;
        std::unique_ptr<entry_type[]> m_entries;

    };  // class map



    //********* Non-Member Function Implementations *********//
    template <Key, V, class Hash, class KeyEqual, class Allocator, class GrowthPolicy>
    [[nodiscard]] constexpr bool operator==(const map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy> &lhs,
                                            const map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy> &rhs)
    { return lhs.size() == rhs.size() && std::equal(lhs.begin(), lhs.end(), rhs.begin()); }


    template <Key, V, class Hash, class KeyEqual, class Allocator, class GrowthPolicy>
    [[nodiscard]] constexpr bool operator!=(const map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy> &lhs,
                                            const map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy> &rhs)
    { return !operator==(lhs, rhs); }


    template <Key, V, class Hash, class KeyEqual, class Allocator, class GrowthPolicy>
    constexpr void swap(const map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy> &lhs,
                        const map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy> &rhs)
    { lhs.swap(rhs); }
}   // namespace dsl::map



#endif //DS_MAP_MAP_H
