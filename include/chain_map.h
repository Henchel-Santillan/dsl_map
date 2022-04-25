#ifndef DSL_CHAIN_MAP_H
#define DSL_CHAIN_MAP_H


#include <compare>
#include <cstddef>
#include <functional>
#include <initializer_list>
#include <iterator>
#include <limits>
#include <memory>
#include <memory_resource>
#include <stdexcept>
#include <utility>


namespace dsl {

    namespace details_map {

        static constexpr float max_load_factor { 1.0f };
        static constexpr std::size_t max_bucket_count { std::numeric_limits<std::size_t>::max() };

        struct growth_policy {
            static constexpr std::size_t compute_index(const std::size_t hash, const std::size_t capacity) {
                return hash & (capacity - 1);
            }

            static constexpr std::size_t compute_closest_capacity(std::size_t min_capacity) {
                constexpr auto max_capacity { std::size_t { 1 } << (std::numeric_limits<std::size_t>::digits - 1) };
                if (min_capacity > max_capacity)
                    return max_capacity;

                min_capacity--;

                for (auto i = 1; i < std::numeric_limits<std::size_t>::digits; i *= 2) 
                    min_capacity |= min_capacity >> i;

                return ++min_capacity;
            }

            static constexpr std::size_t minimum_capacity() {
                return 8u;
            }
        };


        template <class Key, class Value>
        struct chain_map_entry {

            //*** Member Types ***//

            using key_type = Key;
            using mapped_type = Value;
            using value_type = std::pair<const key_type, mapped_type>;


            //*** Member Functions ***//

            chain_map_entry() 
                : m_pair()
                , m_next_entry_local(nullptr)
                , m_next_entry_true(nullptr)
            {}

            value_type m_pair;
            chain_map_entry<Key, Value> *m_next_entry_local;
            chain_map_entry<Key, Value> *m_next_entry_true;
        };
        

        template <class Key, class Value, bool isLocal> 
        class chain_map_const_iterator {
        public:

            //*** Member Types ***//

            using entry_t = chain_map_entry<Key, Value>;

            using iterator_category = std::forward_iterator_tag;
            using difference_type = std::ptrdiff_t;
            using value_type = typename entry_t::value_type;
            using pointer = const value_type*;
            using reference = const value_type&;


            //*** Member Functions ***//

            explicit chain_map_const_iterator(const entry_t *previous) 
                : m_previous(const_cast<entry_t*>(previous)) {}

            [[nodiscard]] constexpr reference operator*() const noexcept {
                auto next = isLocal ? m_previous->m_next_entry_local : m_previous->m_next_entry_true;
                return next->m_pair;
            }

            [[nodiscard]] constexpr pointer operator->() const noexcept {
                auto next = isLocal ? m_previous->m_next_entry_local : m_previous->m_next_entry_true;
                return std::addressof(next->m_pair);
            }

            constexpr chain_map_const_iterator& operator++() noexcept {
                m_previous = isLocal ? m_previous->m_next_entry_local : m_previous->m_next_entry_true;
                return *this;
            }

            constexpr chain_map_const_iterator operator++(int) noexcept {
                chain_map_const_iterator it(*this);
                ++(*this);
                return it;
            }

            [[nodiscard]] constexpr auto operator<=>(const chain_map_const_iterator&) const noexcept = default;

        protected:
            entry_t *m_previous;
        };


        template <class Key, class Value, bool isLocal>
        class chain_map_iterator : public chain_map_const_iterator<Key, Value, isLocal> {
        public:

            //*** Member Types ***//

            using base_t = chain_map_const_iterator<Key, Value, isLocal>;

            using value_type = typename base_t::value_type;
            using reference = value_type&;
            using pointer = value_type*;


            //*** Member Functions ***//

            explicit chain_map_iterator(const base_t::entry_t *previous)
                : base_t(previous) {}

            [[nodiscard]] constexpr reference operator*() const noexcept {
                auto next = isLocal ? m_previous->m_next_entry_local : m_previous->m_next_entry_true;
                return next->m_pair;
            }

            [[nodiscard]] constexpr pointer operator->() const noexcept {
                auto next = isLocal ? m_previous->m_next_entry_local : m_previous->m_next_entry_true;
                return std::addressof(next->m_pair);
            }

            constexpr chain_map_iterator& operator++() noexcept {
                base_t::operator++();
                return *this;
            }

            constexpr chain_map_iterator operator++(int) noexcept {
                chain_map_iterator it(*this);
                ++(*this);
                return it;
            }
        };
        
    }   // namespace details_map



    template <class Key, 
              class Value, 
              class Hash = std::hash<Key>,
              class KeyEqual = std::equal_to<Key>,
              class GrowthPolicy = details_map::growth_policy> 
    class chain_map {
    public:

        //*** Member Types ***//

        using key_type = Key;
        using mapped_type = Value;
        using value_type = std::pair<const Key, Value>;
        using size_type = std::size_t;
        using difference_type = std::ptrdiff_t;

        using hasher = Hash;
        using key_equal = KeyEqual;
        using allocator_type = std::pmr::polymorphic_allocator<std::byte>;

        using reference = value_type&;
        using const_reference = const value_type&;
        using pointer = std::allocator_traits<allocator_type>::pointer;
        using const_pointer = std::allocator_traits<allocator_type>::const_pointer;

        using iterator = typename details_map::chain_map_iterator<Key, Value, false>;
        using const_iterator = typename details_map::chain_map_const_iterator<Key, Value, false>;
        using local_iterator = typename details_map::chain_map_iterator<Key, Value, true>;
        using const_local_iterator = typename details_map::chain_map_const_iterator<Key, Value, true>;


        //*** Member Functions ***//

        //* Constructors *//

        explicit chain_map(allocator_type allocator = {})
            : m_allocator(allocator) 
            , m_size(0)
            , m_bucket_count(GrowthPolicy::minimum_capacity())
            , m_max_load_factor(details_map::max_load_factor)
            , m_hash(Hash())
            , m_equal(key_equal())
            , m_head()
            , m_tail(nullptr)
            , m_entries(nullptr)
        {}

        explicit chain_map(const size_type bucket_count, 
                           const Hash &hash = Hash(),
                           const key_equal &equal = key_equal(),
                           allocator_type allocator = {})
            : chain_map(allocator)
            , m_bucket_count(bucket_count)
            , m_hash(hash)
            , m_equal(equal)
        {}

        chain_map(const size_type bucket_count, 
                  allocator_type allocator = {})
            : chain_map(bucket_count, Hash(), key_equal(), allocator)
        {}

        chain_map(const size_type bucket_count, 
                  const Hash &hash, 
                  allocator_type allocator = {})
            : chain_map(bucket_count, hash, key_equal(), allocator)
        {}

        template <class InputIt>
        chain_map(InputIt first, InputIt last, 
                  const size_type bucket_count = GrowthPolicy::minimum_capacity(),
                  const Hash &hash = Hash(),
                  const key_equal &equal = key_equal(),
                  allocator_type allocator = {})
            : chain_map(bucket_count, hash, equal, allocator)
        { insert(first, last); }

        template <class InputIt>
        chain_map(InputIt first, InputIt last, 
                  const size_type bucket_count, 
                  allocator_type allocator = {})
            : chain_map(first, last, bucket_count, Hash(), key_equal(), allocator)
        {}

        template <class InputIt>
        chain_map(InputIt first, InputIt last, 
                  const size_type bucket_count, 
                  const Hash &hash, 
                  allocator_type allocator = {})
            : chain_map(first, last, bucket_count, hash, key_equal(), allocator) 
        {}

        chain_map(std::initializer_list<value_type> init, 
                  const size_type bucket_count = GrowthPolicy::minimum_capacity(),
                  const Hash &hash = Hash(),
                  const key_equal &equal = key_equal(), 
                  allocator_type allocator = {})
            : chain_map(init.first(), init.last(), bucket_count, hash, equal, allocator) 
        {}

        chain_map(std::initializer_list<value_type> init, 
                  const size_type bucket_count, 
                  allocator_type allocator = {})
            : chain_map(init, bucket_count, Hash(), key_equal(), allocator)
        {}

        chain_map(std::initializer_list<value_type> init, 
                  const size_type bucket_count, 
                  const Hash &hash, 
                  allocator_type allocator = {})
            : chain_map(init, bucket_count, hash, key_equal(), allocator)
        {}


        //* Copy Constructors *//

        chain_map(const chain_map &other,
                  allocator_type allocator = {}) 
            : chain_map(allocator)
        { operator=(other); }

        chain_map(const chain_map &other)
            : chain_map(other, std::allocator_traits<allocator_type>::select_on_container_copy_construction(other.get_allocator()))
        {}


        //* Move Constructors *//

        chain_map(chain_map &&other, 
                  allocator_type allocator = {})
            : chain_map(allocator)
        { operator=(std::move(other)); }

        chain_map(chain_map &&other) 
            : chain_map(other, other.get_allocator())
        {}


        //* Destructor *//
        ~chain_map() {
            erase(begin(), end());
            m_entries = nullptr;
        }


        //* Assignment Operator Overloads *//

        chain_map& operator=(const chain_map&);
        chain_map& operator=(chain_map&&);

        [[nodiscard]] allocator_type get_allocator() const noexcept {
            return m_allocator;
        }


        //* Iterators *//

        iterator begin() noexcept {
            return iterator(m_head.m_next_entry_true);
        }

        const_iterator begin() const noexcept {
            return const_iterator(m_head.m_next_entry_true);
        }

        const_iterator cbegin() const noexcept {
            return const_iterator(m_head.m_next_entry_true);
        }

        iterator end() noexcept {
            return iterator(m_tail);
        }

        const_iterator end() const noexcept {
            return const_iterator(m_tail);
        }

        const_iterator cend() const noexcept {
            return const_iterator(m_tail);
        }


        //* Capacity *//

        [[nodiscard]] bool empty() const noexcept {
            return m_size == 0;
        }

        [[nodiscard]] size_type size() const noexcept {
            return m_size;
        }

        [[nodiscard]] size_type max_size() const noexcept {
            return std::numeric_limits<difference_type>::max();
        }


        //* Modifiers *//

        void clear() noexcept;

        std::pair<iterator, bool> insert(const value_type&);
        std::pair<iterator, bool> insert(value_type&&);

        template <class P>
        std::pair<iterator, bool> insert(P&&);

        iterator insert(const_iterator, const value_type&);
        iterator insert(const_iterator, value_type&&);

        template <class P>
        iterator insert(const_iterator, P&&);

        template <class InputIt>
        void insert(InputIt, InputIt);

        void insert(std::initializer_list<value_type>);

        template <class M>
        std::pair<iterator, bool> insert_or_assign(const Key&, M&&);

        template <class M>
        std::pair<iterator, bool> insert_or_assign(Key&&, M&&);

        template <class M>
        iterator insert_or_assign(const_iterator, const Key&, M&&);

        template <class M>
        iterator insert_or_assign(const_iterator, Key&&, M&&);

        template <class... Args>
        std::pair<iterator, bool> emplace(Args&&...);

        template <class... Args>
        iterator emplace_hint(const_iterator, Args&&...);

        template <class... Args>
        std::pair<iterator, bool> try_emplace(const Key&, Args&&...);

        template <class... Args>
        std::pair<iterator, bool> try_emplace(Key&&, Args&&...);

        template <class... Args>
        iterator try_emplace(const_iterator, const Key&, Args&&...);

        template <class... Args>
        iterator try_emplace(const_iterator, Key&&, Args&&...);

        iterator erase(iterator);
        iterator erase(const_iterator);
        iterator erase(const_iterator, const_iterator);
        size_type erase(const Key&);

        void swap(chain_map&) noexcept(std::allocator_traits<allocator_type>::is_always_equal::value &&
                                       std::is_nothrow_swappable<Hash>::value &&
                                       std::is_nothrow_swappable<key_equal>::value);

        
        //* Lookup *//

        Value& at(const Key&);
        const Value& at(const Key&) const;

        Value& operator[](const Key&);
        Value& operator[](Key&&);

        size_type count(const Key&) const;
        iterator find(const Key &key);
        const_iterator find(const Key &key) const;
        bool contains(const Key&) const;

        std::pair<iterator, iterator> equal_range(const Key&);
        std::pair<const_iterator, const_iterator> equal_range(const Key&) const;


        //* Bucket Interface *//

        local_iterator begin(size_type n) {
            return local_iterator(m_entries[n]);
        }

        const_local_iterator begin(size_type) const {
            return const_local_iterator(m_entries[n]);
        }

        const_local_iterator cbegin(size_type) const {
            return const_local_iterator(m_entries[n]);
        }

        local_iterator end(size_type) {
            return local_iterator(m_entries[node_end_index]);
        }

        const_local_iterator end(size_type) const {
            return const_local_iterator(m_entries[node_end_index]);
        }

        const_local_iterator cend(size_type) const {
            return const_local_iterator(m_entries[node_end_index]);
        }

        size_type bucket_count() const {
            return m_bucket_count;
        }

        size_type max_bucket_count() const {
            return details_map::max_bucket_count;
        }

        size_type bucket_size(size_type n) const {
            auto size = 0;
            auto it = begin(n);
            while (it != end(n)) {
                ++size;
                ++it;
            }
            return size;
        }

        size_type bucket(const Key &key) const {
            return m_hash(key);
        }


        //* Hash Policy *//

        float load_factor() const {
            return size() / bucket_count();
        }

        float max_load_factor() const {
            return m_max_load_factor;
        }

        void max_load_factor(float ml) {
            m_max_load_factor = ml;
        }

        void rehash(size_type);
        void reserve(size_type);


        //* Observers *//

        hasher hash_function() const {
            return m_hash;
        }

        key_equal key_eq() const {
            return m_equal;
        }
        

    private:

        //*** Using Directives ***//

        using node_end_index = std::numeric_limits<difference_type>::max();

        using node_type = typename details_map::chain_map_entry<Key, Value>;
        using GrowthPolicy::compute_index;
        using GrowthPolicy::compute_closest_capacity;
        using GrowthPolicy::minimum_capacity;


        //*** Members ***//

        allocator_type m_allocator;

        size_type m_size;
        size_type m_bucket_count;

        float m_max_load_factor;

        Hash m_hash;
        key_equal m_equal;

        node_type m_head;
        node_type *m_tail;
        node_type **m_entries;

    
        //* Private Functions *//

        template <class M>
        void assign_to_mapped_type(const Key &k, M &&obj);
        
        template <class M>
        void assign_to_mapped_type(Key &&k, M &&obj);
    };


    //****** Member Function Implementations ******//

    //*** Private ***//

    template <class Key, class Value, class Hash, class KeyEqual, class GrowthPolicy>
    template <class M>
    void chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::assign_to_mapped_type(const Key &k, M &&obj) {
        for (auto it = begin(n); it != end(n); ++it) {
            if (key_equal((*it).first, k)) 
                (*it).second = std::forward<M>(obj);
        }        
    }

    template <class Key, class Value, class Hash, class KeyEqual, class GrowthPolicy>
    template <class M>
    void chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::assign_to_mapped_type(Key &&k, M &&obj) {
        assign_to_mapped_type(std::move(k), obj);
    }

    //*** Public ***//

    //* Modifiers *//

    template <class Key, class Value, class Hash, class KeyEqual, class GrowthPolicy>
    void chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::clear() noexcept {
        erase(begin(), end());
    }

    template <class Key, class Value, class Hash, class KeyEqual, class GrowthPolicy>
    std::pair<chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::iterator, bool> chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::insert(const value_type &value) {
        return emplace(value);
    }

    template <class Key, class Value, class Hash, class KeyEqual, class GrowthPolicy>
    std::pair<chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::iterator, bool> chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::insert(value_type &&value) {
        return emplace(std::move(value));
    }    

    template <class Key, class Value, class Hash, class KeyEqual, class GrowthPolicy>
    template <class P>
    std::pair<chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::iterator, bool> chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::insert(P &&value) {
        return emplace(std::forward<P>(value));
    }    

    template <class Key, class Value, class Hash, class KeyEqual, class GrowthPolicy>
    chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::iterator chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::insert(const_iterator hint, const value_type &value) {
        return emplace_hint(hint, value);
    }

    template <class Key, class Value, class Hash, class KeyEqual, class GrowthPolicy>
    chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::iterator chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::insert(const_iterator hint, value_type &&value) {
        return emplace_hint(hint, std::move(value));
    }

    template <class Key, class Value, class Hash, class KeyEqual, class GrowthPolicy>
    template <class P>
    chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::iterator chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::insert(const_iterator hint, P &&value) {
        return emplace_hint(hint, std::forward<P>(value));
    }

    template <class Key, class Value, class Hash, class KeyEqual, class GrowthPolicy>
    template <class InputIt>
    void chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::insert(InputIt first, InputIt last) {
        for (; first != last; ++first) 
            emplace(*first);
    }

    template <class Key, class Value, class Hash, class KeyEqual, class GrowthPolicy>
    void chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::insert(std::initializer_list<value_type> ilist) {
        insert(ilist.begin(), ilist.end());
    }

    template <class Key, class Value, class Hash, class KeyEqual, class GrowthPolicy>
    template <class M>
    std::pair<chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::iterator, bool> chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::insert_or_assign(const Key &k, M &&obj) {
        auto n = m_hash(k);
        if (n >= bucket_count())
            return insert(value_type(k, std::forward<M>(obj)));

        assign_to_mapped_type(k, obj);
    }

    template <class Key, class Value, class Hash, class KeyEqual, class GrowthPolicy>
    template <class M>
    std::pair<chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::iterator, bool> chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::insert_or_assign(Key &&k, M &&obj) {
        auto n = m_hash(k);
        if (n >= bucket_count()) 
            return insert(value_type(std::move(k), std::forward<M>(obj)));

        assign_to_mapped_type(k, obj);
    }

    template <class Key, class Value, class Hash, class KeyEqual, class GrowthPolicy>
    template <class M>
    chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::iterator chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::insert_or_assign(const_iterator hint, const Key &k, M &&obj) {
        auto n = m_hash(k);
        if (n >= bucket_count())
            return (hint, value_type(k, std::forward<M>(obj)));

        assign_to_mapped_type(k, obj);     
    }

    template <class Key, class Value, class Hash, class KeyEqual, class GrowthPolicy>
    template <class M>
    chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::iterator chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::insert_or_assign(const_iterator hint, Key &&k, M &&obj) {
        auto n = m_hash(k);
        if (n >= bucket_count()) 
            return insert(hint, value_type(std::move(k), std::forward<M>(obj)));

        assign_to_mapped_type(k, obj);
    }

    template <class Key, class Value, class Hash, class KeyEqual, class GrowthPolicy>
    template <class... Args>
    std::pair<chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::iterator, bool> chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::emplace(Args &&...args) {
        auto entry = static_cast<entry_t*>(m_allocator.resource()->allocate(sizeof(entry_t), alignof(entry_t)));
    }


    //* Lookup *//

    template <class Key, class Value, class Hash, class KeyEqual, class GrowthPolicy>
    Value& chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::at(const Key &key) {
        auto n = m_hash(key);
        if (n >= bucket_count() >= || m_entries[n] == nullptr)
            throw std::out_of_range("Key does not exist in the chain_map.");
        
        auto it = begin(n);
        while (it != end(n)) {
            if (key_equal(key, (*it).first))
                break; 
            ++it;
        }

        return (*it).second;
    }

    template <class Key, class Value, class Hash, class KeyEqual, class GrowthPolicy>
    const Value& chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::at(const Key &key) const {
        return at(key);
    }

    template <class Key, class Value, class Hash, class KeyEqual, class GrowthPolicy>
    Value& chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::operator[](const Key &key) {
        return try_emplace(key).first->second;
    }

    template <class Key, class Value, class Hash, class KeyEqual, class GrowthPolicy>
    Value& chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::operator[](Key &&key) {
        return try_emplace(std::move(key)).first->second;
    }    

    template <class Key, class Value, class Hash, class KeyEqual, class GrowthPolicy>
    chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::size_type chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::count(const Key &key) const {
        return contains(key) ? 1 : 0;
    }

    template <class Key, class Value, class Hash, class KeyEqual, class GrowthPolicy>
    chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::iterator chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::find(const Key &key) {
        return std::find_if(begin(), end(), [](node_type node) { return key_equal(node->m_pair.first, key); });
    }

    template <class Key, class Value, class Hash, class KeyEqual, class GrowthPolicy>
    chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::const_iterator chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::find(const Key &key) const {
        return std::find_if(begin(), end(), [](node_type node) { return key_equal(node->m_pair.first, key); });        
    }

    template <class Key, class Value, class Hash, class KeyEqual, class GrowthPolicy>
    bool chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::contains(const Key &key) const {
        return find(key) != end();
    }

    template <class Key, class Value, class Hash, class KeyEqual, class GrowthPolicy>
    std::pair<chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::iterator, chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::iterator> chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::equal_range(const Key &key) {
        auto it = find(key);
        return it != end() ? { it, std::next(it) } : { it, it };
    }

    template <class Key, class Value, class Hash, class KeyEqual, class GrowthPolicy>
    std::pair<chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::const_iterator, chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::const_iterator> chain_map<Key, Value, Hash, KeyEqual, GrowthPolicy>::equal_range(const Key &key) {
        auto it = find(key);
        return it != end() ? { it, std::next(it) } : { it, it };
    }


    //* Hash Policy *//


}   // namespace dsl


#endif // DSL_CHAIN_MAP_H