#ifndef DS_MAP_MAP_H
#define DS_MAP_MAP_H


#include <algorithm>
#include <functional>
#include <memory>
#include <memory_resource>
#include <set>
#include <stdexcept>
#include <tuple>

#include "internal/map_iterator.h"
#include "internal/policy.h"
#include "internal/entry.h"
#include "internal/type_attr.h"


namespace dsl::map
{
    namespace details
    {
        static constexpr const float default_max_load_factor = 1.0f;

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
              m_head(),
              m_tail_ptr(nullptr),
              m_entries(m_bucket_count <= 0 ? nullptr : new entry_type*[m_bucket_count])
        {
            rehash(bucket_count);
        }


        // Copy Constructor
        map(const map &rhs,
            allocator_type allocator = {})
            : map() {}

        // Define move ctor, extended move ctor, operator=


        ~map();
        constexpr void swap(const map&) noexcept;


        //*** Access and Iterators ***//
        [[nodiscard]] constexpr size_type size() const noexcept             { return m_size; }
        [[nodiscard]] constexpr size_type max_size() const noexcept         { return std::numeric_limits<size_type>::max(); }
        [[nodiscard]] constexpr size_type bucket_count() const noexcept     { return m_bucket_count; }
        [[nodiscard]] constexpr size_type max_bucket_count() const noexcept { return std::numeric_limits<size_type>::max(); }
        [[nodiscard]] constexpr float load_factor() const noexcept          { return static_cast<float>(m_size) / m_bucket_count; }
        [[nodiscard]] constexpr float max_load_factor() const noexcept      { return m_max_load_factor; }

        constexpr void max_load_factor(float ml)
        {
            if (ml > 0.0f) {
                m_max_load_factor = ml;
                rehash(8u);
            }
        }

        [[nodiscard]] allocator_type get_allocator() const noexcept         { return m_allocator; }

        [[nodiscard]] size_type bucket_size(const size_type n) const
        {
            return (n > m_bucket_count) ? (m_entries[n] != nullptr) ?
                : std::distance(lbegin(n), lend())
                : 0
                : throw std::out_of_range("'n' must satisfy 0 <= n < bucket_count().");
        }

        size_type bucket(const Key &key) const                  { return compute_index(m_hash(key), m_bucket_count); }

        constexpr iterator begin() noexcept                     { return iterator(&m_head); }
        constexpr const_iterator begin() const noexcept         { return const_iterator(&m_head); }
        constexpr const_iterator cbegin() const noexcept        { return begin(); }

        constexpr iterator end() noexcept                       { return iterator(m_tail_ptr); }
        constexpr const_iterator end() const noexcept           { return const_iterator(m_tail_ptr); }
        constexpr const_iterator cend() const noexcept          { return end(); }

        constexpr local_iterator lbegin(const size_type n)
        {
            return (n > m_bucket_count) ?
                throw std::out_of_range("'n' must satisfy 0 <= n < bucket_count().") : iterator(&(m_entries[n]));
        }

        constexpr const_local_iterator lbegin(const size_type n) const
        {
            return (n > m_bucket_count) ?
                throw std::out_of_range("'n' must satisfy 0 <= n < bucket_count().") : const_iterator(&(m_entries[n]));
        }

        constexpr const_local_iterator clbegin(const size_type n) const
        { return lbegin(n); }

        constexpr local_iterator lend() noexcept                { return local_iterator(&m_data[node_end_t]); }
        constexpr const_local_iterator lend() const noexcept    { return const_local_iterator(&m_data[node_end_t]); }
        constexpr const_local_iterator clend() const noexcept   { return lend(); }


        [[nodiscard]] constexpr V& at(const Key&);
        [[nodiscard]] constexpr const V& at(const Key &key) const                 { return at(key); }

        [[nodiscard]] constexpr V& operator[](const Key &key)                     { return try_emplace(key).first->second; }
        [[nodiscard]] constexpr const V& operator[](const Key &key) const         { return operator[](key); }

        constexpr size_type count(const Key &key) const
        { return (std::find_if(begin(), end(), [](entry_type *const curr){ return key_equal(curr->m_key, key); }) != end()) ? 1 : 0; }

        [[nodiscard]] constexpr iterator find(const Key &key)
        { return std::find_if(begin(), end(), [](entry_type *const curr) { return key_equal(curr->m_key, key); }); }

        [[nodiscard]] constexpr const_iterator find(const Key &key) const
        { return std::find_if(begin(), end(), [](entry_type *const curr) { return key_equal(curr->m_key, key); }); }

        [[nodiscard]] constexpr bool contains(const Key &key) const
        { return find(key) != end(); }

        std::pair<iterator, iterator> equal_range(const Key&);
        std::pair<const_iterator, const_iterator> equal_range(const Key&) const;

        constexpr hasher hash_function() const    { return m_hash; }
        constexpr key_equal key_eq() const        { return m_key_equal; }


        //*** Modifiers ***//
        constexpr void rehash(size_type bucket_count);

        std::pair<iterator, bool> insert(const value_type &value)
        { return emplace(value); }

        std::pair<iterator, bool> insert(value_type &&value)
        { return emplace(std::forward<value_type>(value)); }

        template <class ...Args>
        std::pair<iterator, bool> emplace(Args&&...);

        template <class ...Args>
        iterator emplace_hint(const_iterator, Args&&...);

        template <class ...Args>
        std::pair<iterator, bool> try_emplace(const key_type&, Args&&...);

        template <class ...Args>
        std::pair<iterator, bool> try_emplace(key_type&&, Args&&...);

        iterator erase(iterator);
        iterator erase(const_iterator);
        iterator erase(const_iterator, const_iterator);
        size_type erase(const key_type&);


    private:
        //*** Private Using Directives ***//
        using default_max_load_factor = typename details::default_max_load_factor;
        using entry_type = typename details::entry<Key, V>;
        using node_end_t = std::numeric_limits<size_type>::max();

        using GrowthPolicy::compute_index;
        using GrowthPolicy::compute_closest_capacity;
        using GrowthPolicy::minimum_capacity;


        size_type                     m_bucket_count = 0;
        size_type                     m_size = 0;
        key_equal                     m_key_equal;
        Hash                          m_hash;
        float                         m_max_load_factor = default_max_load_factor;
        allocator_type                m_allocator;

        entry_type                    m_head;
        entry_type                    *m_tail_ptr = nullptr;
        entry_type                    **m_entries = nullptr;

    };  // class map



    //********* Member Function Implementations *********//

    template <class Key, class V, class Hash, class KeyEqual, class Allocator, class GrowthPolicy>
    map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::~map()
    {
        erase(begin(), end());
        delete m_entries;
        m_entries = nullptr;
    }

    template <class Key, class V, class Hash, class KeyEqual, class Allocator, class GrowthPolicy>
    constexpr void map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::swap(
            const map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy> &rhs) noexcept
    {
        if (m_allocator == rhs.m_allocator)
        {
            using std::swap;
            swap(rhs.m_bucket_count, m_bucket_count);
            swap(rhs.m_size, m_size);
            swap(rhs.m_key_equal, m_key_equal);
            swap(rhs.m_hash, m_hash);
            swap(rhs.m_max_load_factor, m_max_load_factor);
            swap(rhs.m_head, m_head);
            swap(rhs.m_tail_ptr, m_tail_ptr);
            swap(rhs.m_entries, m_entries);
        }
    }

    template <class Key, class V, class Hash, class KeyEqual, class Allocator, class GrowthPolicy>
    constexpr V& map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::at(const Key &key)
    {
        auto i = m_hash(key);
        if (i > m_bucket_count || m_entries[i] == nullptr)
            throw std::out_of_range("Entry with key does not exist.");

        auto it = lbegin(i);
        while (it != lend())
        {
            if (key_equal(it->m_prev->m_next_bucket->m_key, key))
                break;
            ++it;
        }
        return it->m_prev->m_next_bucket->m_v;
    }

    template <class Key, class V, class Hash, class KeyEqual, class Allocator, class GrowthPolicy>
    constexpr std::pair<map<typename Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::iterator,
    typename map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::iterator>
    map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::equal_range(const Key &key)
    {
        auto it = find(key);
        return (it == end()) ? { it, it } : { it, std::next(it) };
    }

    template <class Key, class V, class Hash, class KeyEqual, class Allocator, class GrowthPolicy>
    constexpr std::pair<typename map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::const_iterator,
    typename map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::const_iterator>
    map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::equal_range(const Key &key) const
    {
        auto it = find(key);
        return (it == end()) ? { it, it } : { it, std::next(it) };
    }

    template <class Key, class V, class Hash, class KeyEqual, class Allocator, class GrowthPolicy>
    constexpr void map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::rehash(const typename map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::size_type bucket_count)
    {
        count = std::max(minimum_capacity(), count);
        count = std::max(count, static_cast<size_type>(m_size / m_max_load_factor));
        count = compute_closest_capacity(count);

        if (count != m_bucket_count)
        {
            auto s { std::set<entry_type *const>{} };
            for (auto it = begin(); it != end(); ++it)
                s.insert(it->m_prev->m_next_true);

            m_bucket_count = count;
            erase(begin(), end());
            delete m_entries;
            m_entries = new entry_type*[m_bucket_count];

            for (std::set<entry_type *const>::iterator iter = s.begin(); iter != s.end(); ++iter)
                emplace({*iter.m_key, *iter.m_v)});
        }
    }

    template <class Key, class V, class Hash, class KeyEqual, class Allocator, class GrowthPolicy>
    template <class ...Args>
    std::pair<typename map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::iterator, bool>
    map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::emplace(Args &&...args)
    {
        auto *node = static_cast<entry_type*>(
                this->m_allocator.resource()->allocate(sizeof(entry_type), alignof(entry_type)));
        try
        {
            this->m_allocator.construct(std::addressof(node), std::forward<Args(args)...);
        }

        catch (...)
        {
            this->m_allocator.resource()->deallocate(node, sizeof(entry_type), alignof(entry_type));
            throw;
        }

        auto index = m_hash(node->m_key);
        if (m_entries[index] == nullptr)
            m_entries[index] = ::operator new (node) entry_type(*(node->m_key), *(node->m_v));
        else
        {
            auto it = lbegin(index);
            while (it != lend())
            {
                if (key_equal(it->m_prev->_bucket->m_key, node->m_key))
                    return { begin() + index, false };  // TODO: map_iterator does not support iterator + (int)
                ++it;
            }
            // [likely out of bounds]
            it->m_prev->m_next_bucket = node;
        }

        this->m_size++;
        return { begin() + index, true };
    }

    template <class Key, class V, class Hash, class KeyEqual, class Allocator, class GrowthPolicy>
    template <class ...Args>
    std::pair<typename map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::iterator, bool>
    map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::emplace_hint(
            typename map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::const_iterator hint,
            Args &&...args)
    {
        auto *node = static_cast<entry_type*>(
                this->m_allocator.resource()->allocate(sizeof(entry_type), alignof(entry_type)));
        try
        {
            this->m_allocator.construct(std::addressof(node), std::forward<Args(args)...);
        }

        catch (...)
        {
            this->m_allocator.resource()->deallocate(node, sizeof(entry_type), alignof(entry_type));
            throw;
        }

        auto *curr = hint->m_prev->m_next_bucket;
        while (curr != nullptr)
        {
            if (key_equal(curr->m_key, node->m_key))
                return { hint, false };
            curr = curr->m_next_bucket;
        }

        curr = node;
        this->m_size++;
        return { hint, true };
    }


    template <class Key, class V, class Hash, class KeyEqual, class Allocator, class GrowthPolicy>
    template <class ...Args>
    std::pair<typename map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::iterator, bool>
    map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::try_emplace(
            const typename map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::key_type &k,
            Args &&...args)
    {
        return emplace(value_type(std::std::piecewise_construct,
                                  std::forward_as_tuple(k),
                                  std::forward_as_tuple(std::forward<Args>(args)...)));
    }

    template <class Key, class V, class Hash, class KeyEqual, class Allocator, class GrowthPolicy>
    template <class ...Args>
    std::pair<typename map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::iterator, bool>
    map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::try_emplace(
            typename map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::key_type &&k,
            Args &&...args)
    {
    return emplace(value_type(std::std::piecewise_construct,
                              std::forward_as_tuple(std::move(k)),
                              std::forward_as_tuple(std::forward<Args>(args)...)));
    }

    // Erase methods incorrect: does not search separate chaining
    // TODO: erase with local_iterator
    template <class Key, class V, class Hash, class KeyEqual, class Allocator, class GrowthPolicy>
    typename map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::iterator
    map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::erase(typename map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::iterator pos)
    {
        iterator it = pos;
        return erase(static_cast<const_iterator>(pos), static_cast<const_iterator>(++it));
    }

    template <class Key, class V, class Hash, class KeyEqual, class Allocator, class GrowthPolicy>
    typename map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::iterator
    map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::erase(typename map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::const_iterator pos)
    {
        const_iterator it = pos;
        return erase(pos, ++it);
    }

    template <class Key, class V, class Hash, class KeyEqual, class Allocator, class GrowthPolicy>
    typename map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::iterator
    map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::erase(typename map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::const_iterator start,
                                                                typename map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::const_iterator finish)
    {
        auto *b = start.m_prev->m_next_true, *e = finish.m_prev->m_next_true;
        if (e == nullptr)
            m_tail = start.m_prev;
        start.m_prev->m_next_true = e;

        auto i = 0;

        while (b != e)
        {
            auto *curr = b;
            b = b->m_next_true;
            this->m_size--;

            if (key_equal(curr->m_key, m_entries[i]->m_key))
            {
                delete curr;    // implicitly calls ::operator delete
                i++;
            }

            std::allocator_traits<allocator_type>::destroy(this->m_allocator, std::addressof(*curr));
            this->m_allocator.resource()->deallocate(curr, sizeof(entry_type), alignof(entry_type));
        }

        return static_cast<const_iterator>(start);
    }

    template <class Key, class V, class Hash, class KeyEqual, class Allocator, class GrowthPolicy>
    typename map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::iterator
    map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::erase(const typename map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy>::key_type &k)
    {
        auto index = m_hash(k);
        auto local_it = lbegin(index);
        while (local_it != lend())
        {
            if (key_equal(local_it->m_prev->m_next_bucket->m_key, k))
                delete curr;
            ++local_it;
        }
        std::allocator_traits<allocator_type>::destroy(this->m_allocator, std::addressof(*(local_it->m_prev->m_next_bucket)));
        this->m_allocator.resource()->deallocate(local_it->m_prev->m_next_bucket, sizeof(entry_type), alignof(entry_type));
        this->m_size--;

        auto it = begin();
        auto c = 0;
        while (c != index)
        {
            ++it;
            c++;
        }

        return it;
    }


    //********* Non-Member Function Implementations *********//
    template <class Key, class V, class Hash, class KeyEqual, class Allocator, class GrowthPolicy>
    [[nodiscard]] constexpr bool operator==(const map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy> &lhs,
                                            const map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy> &rhs)
    { return lhs.size() == rhs.size() && std::equal(lhs.begin(), lhs.end(), rhs.begin()); }


    template <class Key, class V, class Hash, class KeyEqual, class Allocator, class GrowthPolicy>
    [[nodiscard]] constexpr bool operator!=(const map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy> &lhs,
                                            const map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy> &rhs)
    { return !operator==(lhs, rhs); }


    template <Key, V, class Hash, class KeyEqual, class Allocator, class GrowthPolicy>
    constexpr void swap(const map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy> &lhs,
                        const map<Key, V, Hash, KeyEqual, Allocator, GrowthPolicy> &rhs)
    { lhs.swap(rhs); }
}   // namespace dsl::map



#endif //DS_MAP_MAP_H
