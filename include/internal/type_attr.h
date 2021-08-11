#ifndef DS_MAP_TYPE_ATTR_H
#define DS_MAP_TYPE_ATTR_H


#include <type_traits>


namespace dsl::map::details
{
    // Possible implementation, according to
    // https://en.cppreference.com/w/cpp/experimental/nonesuch
    struct nonesuch
    {
        ~nonesuch() = delete;
        nonesuch(const nonesuch&) = delete;
        void operator=(const nonesuch&) = delete;

    };  // struct nonesuch

    namespace internal
    {
        // Possible implementation, according to
        // https://en.cppreference.com/w/cpp/experimental/is_detected
        template <class Default, class AlwaysVoid, template<class...> class Op, class... Args>
        struct detector
        {
            using value_t = std::false_type;
            using type = Default;
        };

        template <class Default, template<class ...> class Op, class... Args>
        struct detector<Default, std::void_t<Op<Args...>>, Op, Args...>
        {
            using value_t = std::true_type;
            using type = Op<Args...>;
        };

    }   // namespace internal

    template<template<class...> class Op, class... Args>
    using is_detected = typename internal::detector<nonesuch, void, Op, Args...>::value_t;

    template<template<class...> class Op, class... Args>
    using detected_t = typename internal::detector<nonesuch, void, Op, Args...>::type;

    template<class Default, template<class...> class Op, class... Args>
    using detected_or = internal::detector<Default, void, Op, Args...>;

}   // namespace dsl::map::details


#endif //DS_MAP_TYPE_ATTR_H
