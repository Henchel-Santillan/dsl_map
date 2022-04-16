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
            using value_type = std::pair<const Key, Value>;


            //*** Member Functions ***//

            chain_map_entry() 
                : m_key()
                , m_value()
                , m_next_entry(nullptr)
                , m_next_bucket(nullptr) 
            {}

            chain_map_entry(const Key &key, const Value &value) 
                : m_key(key)
                , m_value(value)
                , m_next_entry(nullptr)
                , m_next_bucket(nullptr)
            {}

            std::pair<Key, Value> to_pair() {
                return { m_key, m_value };
            }

            Key m_key;
            Value m_value;
            chain_map_entry<Key, Value> *m_next_entry, *m_next_bucket;
        };


        template <class Key, class Value, bool isLocal>
        class chain_map_const_iterator {
        public:

            //*** Member Types ***//

            using entry_t = chain_map_entry<Key, Value>;
            using pair_t = typename chain_map_entry<Key, Value>::pair_t; 

            using iterator_category = std::forward_iterator_tag;
            using difference_type = std::ptrdiff_t;
            using value_type = pair_t;
            using pointer = const value_type*;
            using reference = const value_type&;


            //*** Member Functions ***//
            
            explicit chain_map_const_iterator(const entry_t *curr)
                ; m_curr(const_cast<entry_t*>(curr)) {}

            [[nodiscard]] constexpr reference operator*() const noexcept {
                return m_curr->to_pair();
            }

            [[nodiscard]] constexpr pointer operator->() const noexcept {
                return std::addressof(m_curr->to_pair());
            }

            constexpr chain_map_const_iterator& operator++() noexcept {
                m_curr = (isLocal) ? m_curr->m_next_entry : m_curr->m_next_bucket;
                return *this;
            }

            constexpr chain_map_const_iterator operator++(int) noexcept {
                chain_map_const_iterator it(*this);
                ++(*this);
                return it;
            }

            [[nodiscard]] constexpr auto operator<=>(const chain_map_const_iterator&) const noexcept = default;

        protected:
            entry_t *m_curr;
        };


        template <class Key, class Value, bool isLocal>
        class chain_map_iterator : public chain_map_const_iterator<Key, Value, isLocal> {
        public:

            //*** Member Types ***//

            using base_t = chain_map_const_iterator<Key, Value, isLocal>
            using value_type = typename base_t::value_type;
            using pointer = value_type*;
            using reference = value_type&;           


            //*** Member Functions ***//
 
            explicit chain_map_iterator(const base_t::entry_t *curr)
                : base_t(curr) {}

            [[nodiscard]] constexpr reference operator*() const noexcept {
                return this->m_curr->toPair();
            }

            [[nodiscard]] constexpr pointer operator->() const noexcept {
                return std::addressof(this->m_curr->toPair());
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
        using allocator_type = std::pmr::polymorphic_allocator<value_type>;

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
            , m_bucket_interface(static_cast<value_type*>(m_allocator.resource()->allocate(sizeof(value_type) * m_bucket_count, alignof(value_type))))
            , m_max_load_factor(1.0) 
            , m_hash(Hash())
            , m_equal(key_equal())
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
            : chain_map(other. other.get_allocator())
        {}


        //* Destructor *//
        ~chain_map() {
            erase(begin(), end());
        }


        //* Assignment Operator Overloads *//

        chain_map& operator=(const chain_map&);
        chain_map& operator=(chain_map&&);

        [[nodiscard]] allocator_type get_allocator() const noexcept {
            return m_allocator;
        }


        //* Iterators *//

        iterator begin() noexcept {
            return iterator(m_bucket_interface[0]);
        }

        const_iterator begin() const noexcept {
            return const_iterator(m_bucket_interface[0]);
        }

        const_iterator cbegin() const noexcept {
            return const_iterator(m_bucket_interface[0]);
        }

        iterator end() noexcept {
            return iterator(m_bucket_interface[m_bucket_count]);
        }

        const_iterator end() const noexcept {
            return const_iterator(m_bucket_interface[m_bucket_count]);
        }

        const_iterator cend() const noexcept {
            return const_iterator(m_bucket_interface[m_bucket_count]);
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
        bool contains(const Key&) const;

        std::pair<iterator, iterator> equal_range(const Key&);
        std::pair<const_iterator, const_iterator> equal_range(const Key&) const;


        //* Bucket Interface *//

        local_iterator begin(size_type);
        const_local_iterator begin(size_type) const;
        const_local_iterator cbegin(size_type) const;

        local_iterator end(size_type);
        const_local_iterator end(size_type) const;
        const_local_iterator cend(size_type) const;

        size_type bucket_count() const;
        size_type max_bucket_count() const;
        size_type bucket_size(size_type) const;
        size_type bucket(const Key&) const;


        //* Hash Policy *//

        float load_factor() const;
        float max_load_factor() const;
        void max_load_factor(float ml);
        void rehash(size_type);
        void reserve(size_type);


        //* Observers *//

        hasher hash_function() const;
        key_equal key_eq() const;
        

    private:
        allocator_type m_allocator;

        size_type m_size;
        size_type m_bucket_count;

        value_type *m_bucket_interface;

        float m_max_load_factor;

        Hash m_hash;
        key_equal m_equal;
    };

}   // namespace dsl


#endif // DSL_CHAIN_MAP_H