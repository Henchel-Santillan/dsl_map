#ifndef DS_MAP_MAP_ITERATOR_H
#define DS_MAP_MAP_ITERATOR_H


#include <cstddef>
#include <iterator>
#include <type_traits>

#include "entry.h"


namespace dsl::map::iterators
{
    template <class Key, class V>
    using entry = details::entry<Key, V>;

    template <class Key,
              class V,
              bool isConst,
              bool isLocal
              >
    class map_iterator
    {
    public:
        using iterator_category = std::forward_iterator_tag;
        using difference_type = std::ptrdiff_t;
        using value_type = std::conditional_t<isConst, const pair_type, pair_type>
        using pointer = value_type*;
        using reference = value_type&;

        constexpr explicit map_iterator(entry *prev) noexcept
            : m_prev(prev) {}

        [[nodiscard]] constexpr reference operator*() const noexcept
        { return (isLocal) ? m_prev->m_next_bucket->pair_t : m_prev->m_next_true->pair_t; }

        [[nodiscard]] constexpr pointer operator->() const noexcept
        {
            return (isLocal) ? std::addressof(m_prev->m_next_bucket->pair_t) :
            std::addressof(m_prev->m_next_true->pair_t);
        }

        constexpr map_iterator& operator++() noexcept
        {
            m_prev = (isLocal) ? m_prev->m_next_bucket : m_prev->m_next_true;
            return *this;
        }

        constexpr map_iterator operator++(int) noexcept
        {
            map_iterator temp(*this);
            ++(*this);
            return temp;
        }

        [[nodiscard]] constexpr auto operator<=>(const map_iterator&) noexcept = default;


    private:
        entry *m_prev = nullptr;
        using pair_type = std::pair<const Key, V>;

    };  // class map_iterator

}   //namespace dsl::map::details


#endif //DS_MAP_MAP_ITERATOR_H
