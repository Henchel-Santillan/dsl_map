#ifndef DS_MAP_ENTRY_H
#define DS_MAP_ENTRY_H


#include <utility>


namespace dsl::map::details
{
    template <class Key, class V>
    struct entry
    {
        using pair_t = std::pair<const Key, V>;

        //*** Member Functions ***//

        // Constructors
        entry() noexcept = default;
        entry(const Key &key, const V &v) noexcept
        : m_key(key),
          m_v(v),
          m_next_bucket(nullptr),
          m_next_true(nullptr) {}

        // Move assignment
        entry& operator=(entry &&rhs) noexcept
        {
            if (this != &rhs)
                swap(rhs);
            return *this;
        }

        ~entry() noexcept = default;

        constexpr void swap(entry &rhs) noexcept
        {
            using std::swap;
            swap(rhs.m_key, m_key);
            swap(rhs.m_v, m_v);
            swap(rhs.m_next_bucket, m_next_bucket);
            swap(rhs.m_next_true, m_next_true);
        }

        Key m_key;
        V m_v;
        entry<Key, V> *m_next_bucket, *m_next_true;

    };  // struct entry

    template <class Key, class V>
    constexpr void swap(entry<Key, V> &lhs, entry<Key, V> &rhs) noexcept
    { lhs.swap(rhs); }

}   // namespace dsl::map::details


#endif //DS_MAP_ENTRY_H
