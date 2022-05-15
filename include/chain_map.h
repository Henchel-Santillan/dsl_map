#ifndef DSL_CHAIN_MAP_H
#define DSL_CHAIN_MAP_H


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

        static const float max_load_factor { 1.0f };
        static const std::size_t max_bucket_count { std::numeric_limits<std::size_t>::max() };

        struct growth_policy {
            static constexpr std::size_t compute_index(const std::size_t hash, const std::size_t capacity) {
                return hash & (capacity - 1);
            }

            static constexpr std::size_t compute_closest_capacity(std::size_t min_capacity) {
                auto max_capacity { std::size_t { 1 } << (std::numeric_limits<std::size_t>::digits - 1) };
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


        template <class Key, class Tp>
        struct map_entry {

            //*** Member Types ***//

            using key_type = Key;
            using mapped_type = Tp;
            using value_type = std::pair<const key_type, mapped_type>;


            //*** Member Functions ***//

            map_entry() 
                : m_pair() 
                , m_next_true(nullptr) 
                , m_next_local(nullptr)
            {}
            
            value_type m_pair;
            map_entry<Key, Tp> *m_next_true;
            map_entry<Key, Tp> *m_next_local;
        };


        template <class Key, class Tp, bool isLocal>
        class chain_map_const_iterator {
        public:

            //*** Member Types ***//

            using iterator_category = std::forward_iterator_tag;
            using difference_type = std::ptrdiff_t;
            using value_type = std::pair<const Key, Tp>;
            using pointer = const value_type*;
            using reference = const value_type&;


            //*** Member Functions ***//

            explicit chain_map_const_iterator(const map_entry<Key, Tp> *previous)
                : m_previous(previous) {}

            [[nodiscard]] reference operator*() const noexcept {
                return isLocal ? m_previous->m_next_local->m_pair : m_previous->m_next_true->m_pair;
            }

            [[nodiscard]] pointer operator->() const noexcept {
                return isLocal ? std::addressof(m_previous->m_next_local->m_pair) : std::addressof(m_previous->m_next_true->m_pair);
            }

            chain_map_const_iterator& operator++() noexcept {
                m_previous = isLocal ? m_previous->m_next_local : m_previous->m_next_true;
                return *this;
            }

            chain_map_const_iterator operator++(int) noexcept {
                chain_map_const_iterator it(*this);
                ++(*this);
                return it;
            }

            bool operator==(const chain_map_const_iterator &other) const noexcept {
                return m_previous == other.m_previous;
            }

            bool operator!=(const chain_map_const_iterator &other) const noexcept {
                return !operator==(other);
            }

        protected:
            map_entry<Key, Tp> *m_previous;
        };


        template <class Key, class Tp, bool isLocal>
        class chain_map_iterator : public chain_map_const_iterator {
        public:

            //*** Member Types ***//

            using base_t = chain_map_const_iterator<Key, Tp, isLocal>;
            using value_type = typename base_t::value_type;
            using pointer = value_type*;
            using reference = value_type&;


            //*** Member Functions ***//

            explicit chain_map_iterator(const map_entry<Key, Tp> *previous) 
                : base_t(previous) {}

            [[nodiscard]] reference operator*() const noexcept {
                return base_t::operator*();
            }

            [[nodiscard]] pointer operator->() const noexcept {
                return base_t::operator->();
            }

            chain_map_iterator& operator++() noexcept {
                base_t::operator++();
                return *this;
            }

            chain_map_iterator operator++(int) noexcept {
                chain_map_iterator it(*this);
                ++(*this);
                return it;
            }
        };

    }   // namespace details_map


    template <class Key, class Tp, class Hash = std::hash<Key>, class KeyEqual = std::equal_to<Key>, class GrowthPolicy = typename details_map::growth_policy>
    class chain_map {
    public:

        //*** Member Types ***//

        using key_type = Key;
        using mapped_type = Tp;
        using value_type = std::pair<const Key, Tp>;
        
        using size_type = std::size_t;
        using difference_type = std::diff_t;
        using hasher = Hash;
        using key_equal = KeyEqual;

        using allocator_type = std::pmr::polymorphic_allocator<std::byte>;
        using reference = value_type&;
        using const_reference = const value_type&;
        using pointer = std::allocator_traits<allocator_type>::pointer;
        using const_pointer = std::allocator_traits<allocator_type>::const_pointer;

        using iterator = typename details_map::chain_map_iterator<Key, Tp, false>;
        using const_iterator = typename details_map::chain_map_const_iterator<Key, Tp, false>;
        using local_iterator = typename details_map::chain_map_iterator<Key, Tp, true>;
        using const_local_iterator = typename details_map::chain_map_const_iterator<Key, Tp, true>;


        //*** Member Functions ***//

        //* Constructors *//

        explicit chain_map(allocator_type allocator = {})
            : m_allocator(allocator) 
            , m_size(0)
            , m_bucket_count(minimum_capacity())
            , m_max_load_factor(details_map::max_load_factor)
            , m_hasher(Hash())
            , m_equal(key_equal())
            , m_head()
            , m_tail(nullptr)
            , m_entries(new entry_t*[m_bucket_count])
        {}

        explicit chain_map(const size_type bucket_count, 
                           const Hash &hash = Hash(),
                           const key_equal &equal = key_equal(),
                           allocator_type allocator = {})
            : chain_map(allocator)
            , m_bucket_count(bucket_count)
            , m_hasher(hash)
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
                  const size_type bucket_count = minimum_capacity(),
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
                  const size_type bucket_count = minimum_capacity(),
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
            clear();
            delete[] m_entries;
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

        void clear();

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

        Tp& at(const Key&);
        const Tp& at(const Key&) const;

        Tp& operator[](const Key&);
        Tp& operator[](Key&&);

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

        const_local_iterator begin(size_type n) const {
            return const_local_iterator(m_entries[n]);
        }

        const_local_iterator cbegin(size_type n) const {
            return const_local_iterator(m_entries[n]);
        }

        local_iterator end(size_type) {
            return local_iterator(m_entries[bucket_end_index]);
        }

        const_local_iterator end(size_type) const {
            return const_local_iterator(m_entries[bucket_end_index]);
        }

        const_local_iterator cend(size_type) const {
            return const_local_iterator(m_entries[bucket_end_index]);
        }

        size_type bucket_count() const {
            return m_bucket_count;
        }

        size_type max_bucket_count() const {
            return details_map::max_bucket_count;
        }

        size_type bucket_size(size_type) const;

        size_type bucket(const Key &key) const {
            return m_hasher(key);
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
            return m_hasher;
        }

        key_equal key_eq() const {
            return m_equal;
        }


    private:

        //*** Using Directives ***//

        using bucket_end_index = std::numeric_limits<size_type>::max();
        using entry_t = typename details_map::map_entry<Key, Tp>;

        using GrowthPolicy::compute_index;
        using GrowthPolicy::compute_closest_capacity;
        using GrowthPolicy::minimum_capacity;


        //*** Members ***//

        allocator_type m_allocator;

        size_type m_size;           // Total number of entries
        size_type m_bucket_count;   // Number of "buckets" or sub-containers

        float m_max_load_factor;

        hasher m_hasher;
        key_equal m_equal;

        entry_t   m_head;
        entry_t  *m_tail;
        entry_t **m_entries;


        //*** Functions ***//

        template <class M>
        std::pair<iterator, bool> assign_to_mapped_type(const Key&, size_type, M&&);

        void reallocate_exactly(size_type);
    };



    //****** Member Function Implementations ******//


    //*** Private ***//

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    template <class M>
    std::pair<typename chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::iterator, bool> chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::assign_to_mapped_type(const Key &k, size_type hash, M &&obj) {
        auto it = m_entries[n];
        for (auto local_it = begin(n); local_it != end(n); ++local_it) {
            if (m_equal(k, (*it).first)) {
                (*it).second = std::forward<M>(obj);
                break;
            }
            ++it;
        }
        return { it, true };
    }

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    void chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::reallocate_exactly(size_type min_size) {
        auto new_cap = compute_closest_capacity(min_size);
        auto data = new entry_t*[new_cap];

        for (auto i = 0; i < bucket_count(); ++i) 
            data[i] = m_entries[i];

        delete[] m_entries;
        m_entries = data;
        m_bucket_count = new_cap;
    }


    //*** Public ***//

    //* Modifiers *//

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    void chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::clear() {
        erase(begin(), end());
    }

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    std::pair<typename chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::iterator, bool> chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::insert(const value_type &value) {
        return emplace(value);
    }

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    std::pair<typename chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::iterator, bool> chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::insert(value_type &&value) {
        return emplace(std::move(value));
    }    

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    template <class P>
    std::pair<typename chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::iterator, bool> chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::insert(P &&value) {
        return emplace(std::forward<P>(value));
    }    

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    typename chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::iterator chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::insert(const_iterator hint, const value_type &value) {
        return emplace_hint(hint, value);
    }

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    typename chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::iterator chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::insert(const_iterator hint, value_type &&value) {
        return emplace_hint(hint, std::move(value));
    }

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    template <class P>
    typename chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::iterator chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::insert(const_iterator hint, P &&value) {
        return emplace_hint(hint, std::forward<P>(value));
    }

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    template <class InputIt>
    void chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::insert(InputIt first, InputIt last) {
        for (; first != last; ++first) 
            emplace(*first);
    }

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    void chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::insert(std::initializer_list<value_type> ilist) {
        insert(ilist.begin(), ilist.end());
    }

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    template <class M>
    std::pair<typename chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::iterator, bool> chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::insert_or_assign(const Key &k, M &&obj) {
        auto n = m_hasher(k);
        if (n >= bucket_count() || m_entries[n] == nullptr)
            return insert(value_type(k, std::forward<M>(obj)));
        return assign_to_mapped_type(k, n, obj);
    } 

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    template <class M>
    std::pair<typename chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::iterator, bool> chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::insert_or_assign(Key &&k, M &&obj) {
        auto n = m_hasher(k);
        if (n >= bucket_count() || m_entries[n] == nullptr) 
            return insert(value_type(std::move(k), std::forward<M>(obj)));
        return assign_to_mapped_type(std::move(k), n, obj);
    }

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    template <class M>
    typename chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::iterator chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::insert_or_assign(const_iterator hint, const Key &k, M &&obj) {
        auto n = m_hasher(k);
        if (n >= bucket_count() || m_entries[n] == nullptr) 
            return insert(hint, value_type(k, std::forward<M>(obj)));
        return assign_to_mapped_type(k, n, obj);
    }

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    template <class M>
    typename chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::iterator chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::insert_or_assign(const_iterator hint, Key &&k, M &&obj) {
        auto n = m_hasher(k);
        if (n >= bucket_count() || m_entries[n] == nullptr)
            return insert(hint, value_type(std::move(k), std::forward<M>(obj)));
        return assign_to_mapped_type(std::move(k), n, obj);
    }

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    template <class ...Args>
    std::pair<typename chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::iterator, bool> chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::emplace(Args &&...args) {
        if (size() + 1 >= max_size()) 
            throw std::length_error("Call to emplace() will result in container size exceeding implementation-defined size limit.");

        auto entry = static_cast<entry_t*>(m_allocator.resource()->allocate(sizeof(entry_t), alignof(entry_t)));

        try {
            m_allocator.construct(std::addressof(entry->m_pair), std::forward<Args>(args)...);
        } catch (...) {
            m_allocator.resource()->deallocate(entry, sizeof(entry_t), alignof(entry_t));
            throw;
        }

        // Find where to insert the in-place constructed entry in the chain_map.
        // If hash is not in bounds, reallocate the entry buffer
        auto n = m_hasher(entry->m_pair.first);
        if (bucket_count() <= n && n < max_bucket_count())
            reallocate_exactly(n);

        // Number of entries in the chain_map up to the given bucket found after hashing the key
        auto entries_upto = 0;
        for (auto i = 0; i < n; ++i) 
            entries_upto += bucket_size(i);
        
        auto it = std::advance(begin(), entries_upto);

        if (m_entries[n] != nullptr) {  // implies a local head exists, since pointer to beginning of SLL at bucket n is non-null
            for (auto local_it = begin(n); local_it != end(n); ++local_it) {
                if (key_equal((*it).first, entry->m_pair.first)) {
                    
                    // Destroy and deallocate the in-place constructed entry if an entry with the same key already exists in the map
                    std::allocator_traits<allocator_type>::destroy(entry, std::addressof(entry->m_pair));
                    m_allocator.resource()->deallocate(entry, sizeof(entry_t), alignof(entry_t));
                    return { it, false };
                }
                ++it;
            }

            // Update local "next" entry pointers
            entry->m_next_local = it.m_previous->m_next_local;
            it.m_previous->m_next_local = entry;

        } else m_entries[n] = entry;

        // Update next "true" entry pointers
        entry->m_next_true = it.m_previous->m_next_true;
        it.m_previous->m_next_true = entry;

        if (it.m_previous == m_tail) 
            m_tail = entry;

        ++m_size;

        // After inserting the entry, rehash if needed
        if (size() > max_load_factor() * bucket_count())
            rehash();
        
        return { it, true };
    }

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    template <class... Args>
    typename chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::iterator chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::emplace_hint(const_iterator /* hint */, Args &&...args) {
        return emplace(std::forward<Args>(args)...).first;
    }

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    template <class... Args>
    std::pair<typename chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::iterator, bool> chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::try_emplace(const Key &k, Args &&...args) {
        return emplace(value_type(std::piecewise_construct, std::forward_as_tuple(k), std::forward_as_tuple(std::forward<Args>(args)...)));
    }

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    template <class... Args>
    std::pair<typename chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::iterator, bool> chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::try_emplace(Key &&k, Args &&...args) {
        return emplace(value_type(std::piecewise_construct, std::forward_as_tuple(std::move(k)), std::forward_as_tuple(std::forward<Args>(args)...)));
    }

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    template <class... Args>
    typename chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::iterator chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::try_emplace(const_iterator hint, const Key &k, Args &&...args) {
        return emplace_hint(hint, value_type(std::piecewise_construct, std::forward_as_tuple(k), std::forward_as_tuple(std::forward<Args>(args)...)));
    }

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    template <class... Args>
    typename chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::iterator chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::try_emplace(const_iterator hint, Key &&k, Args &&...args) {
        return emplace_hint(hint, value_type(std::piecewise_construct, std::forward_as_tuple(std::move(k)), std::forward_as_tuple(std::forward<Args>(args)...)));
    }

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    typename chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::iterator chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::erase(iterator pos) {
        return erase(static_cast<const_iterator>(pos));
    }

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    typename chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::iterator chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::erase(const_iterator pos) {
        return erase(pos, std::next(pos));
    }

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    typename chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::iterator chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::erase(const_iterator first, const_iterator last) {
        auto entry_first = first.m_previous->m_next;
        auto entry_last = last.m_previous->m_next;

        if (entry_last == nullptr) 
            m_tail = entry_first.m_prev;

        entry_first.m_prev->m_next = entry_last;

        while (entry_first != entry_last) {
            auto entry_old = entry_first;
            entry_first = entry_first->m_next;
            --m_size;
            std::allocator_traits<allocator_type>::destroy(m_allocator, std::addressof(entry_old->m_pair));
            m_allocator.resource()->deallocate(entry_old, sizeof(entry_t), alignof(entry_t));
        }

        return dynamic_cast<iterator>(first);
    }

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    void chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::swap(chain_map &other) noexcept(std::allocator_traits<Allocator>::is_always_equal::value &&
                                                                                           std::is_nothrow_swappable<Hash>::value &&
                                                                                           std::is_nothrow_swappable<key_equal>::value) {
        if (m_allocator == other.m_allocator) {
            auto tail = other.empty() ? &m_head : other.m_tail;
            auto tail_other = empty() ? &other.m_head : m_tail;

            using std::swap;
            swap(m_size, other.m_size);
            swap(m_bucket_count, other.m_bucket_count);
            swap(m_max_load_factor, other.m_max_load_factor);

            swap(m_hasher, other.m_hasher);
            swap(m_equal, other.m_equal);

            swap(m_head.m_next_true, other.m_head.m_next_true);
            swap(m_entries, other.m_entries);

            m_tail = tail;
            other.m_tail = other.m_tail;
        }                                                                                    
    }


    //* Lookup *//

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    Tp& chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::at(const Key &k) {
        auto n = m_hasher(k);
        if (n >= bucket_count() || m_entries[n] == nullptr) 
            throw std::out_of_range("Key does not exist in the chain_map.");

        for (auto it = begin(n); it != end(n); ++it) {
            if (m_equal(k, (*it).first)) 
                break;
        }

        return (*it).second;
    }

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    const Tp& chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::at(const Key &k) const {
        return at(k);
    }

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    Tp& chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::operator[](const Key &k) {
        return try_emplace(k).first->second;
    }

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    Tp& chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::operator[](Key &&k) {
        return try_emplace(std::move(k)).first->second;
    }    

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    typename chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::size_type chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::count(const Key &k) const {
        return contains(k) ? 1 : 0;
    }

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    typename chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::iterator chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::find(const Key &k) {
        return std::find_if(begin(), end(), [](entry_t *entry) { return m_equal(entry->m_pair.first, k); });
    }

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    typename chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::const_iterator chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::find(const Key &k) const {
        return std::find_if(begin(), end(), [](entry_t *entry) { return m_equal(entry->m_pair.first, k); });        
    }

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    bool chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::contains(const Key &k) const {
        return find(k) != end();
    }

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    std::pair<typename chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::iterator, typename chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::iterator> chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::equal_range(const Key &k) {
        auto it = find(k);
        return it != end() ? { it, std::next(it) } : { it, it };
    }

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    std::pair<typename chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::const_iterator, typename chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::const_iterator> chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::equal_range(const Key &k) const {
        auto it = find(k);
        return it != end() ? { it, std::next(it) } : { it, it };
    }

    
    //* Bucket Interface *//

    template <class Key, class Tp, class Hash, class KeyEqual, class GrowthPolicy>
    typename chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::size_type chain_map<Key, Tp, Hash, KeyEqual, GrowthPolicy>::bucket_size(size_type n) const {
        if (n >= bucket_count())
            return 0;
        
        auto size = 0;
        for (auto it = begin(n); it != end(n); ++it)   
            ++size;
        return size;
    }

}   // namespace dsl


#endif // DSL_CHAIN_MAP_H