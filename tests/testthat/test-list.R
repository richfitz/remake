context("lists")

## TODO: test zero length lists

## TODO: change in list attributes (names at least) should render a
## list object entirely changed?
##  - better to pull the names out and reapply them via remake?
##  - if doing that then other attributes can be ignored as they never
##    make it through to the function as [[ drops them.

## There is a lot in here before I start splitting this up into
## sensible units.  There are good chunks here that can be tested
## separately, but for now it's one big regression type test.
test_that("lists", {
  cleanup()
  obj <- remake("remake_list.yml")

  expect_that(obj$targets$mylist$list, is_true())
  expect_that(obj$targets$mylist2$list, is_true())
  expect_that(obj$targets$mylist2$each, equals("mylist"))
  expect_that(obj$targets$notlist$list, is_false())
  expect_that(obj$targets$kindalist$list, is_true())

  mylist <- remake_make(obj, "mylist")

  expect_that(obj$store$objects$type("mylist"), equals("list"))
  expect_that(obj$store$objects$length_list("mylist"), equals(5L))

  curr <- target_is_current(obj$targets$mylist2, obj$store)
  expect_that(curr, is_false())
  expect_that(attr(curr, "each"), is_null()) # no list information here

  mylist2 <- remake_make(obj, "mylist2")
  expect_that(mylist2, equals(lapply(mylist, length)))
  expect_that(names(mylist2), equals(names(mylist)))

  curr <- target_is_current(obj$targets$mylist2, obj$store)
  expect_that(curr, is_true())
  expect_that(attr(curr, "each"), is_null()) # no list information here

  dep <- obj$store$db$get("mylist2")$depends
  expect_that(names(dep), equals("mylist"))
  expect_that(attr(dep, "each"),
              equals(obj$store$objects$get_list_hash("mylist")))

  ## Now, let's change the value of a single one of these.  Doing this
  ## outside of the usual remake approach for now; not ideal but it
  ## does trigger the correct downstream behaviour.
  obj$store$objects$set_list_element("mylist", 2, runif(3))

  ## yay:
  curr <- target_is_current(obj$targets$mylist2, obj$store)
  expect_that(curr, is_false())
  each <- attr(curr, "each")
  expect_that(each, not(is_null()))
  expect_that(each, equals(seq_along(mylist) != 2))

  ## So, how do we get that to pass along as remake runs its checks...
  mylist2.2 <- remake_make(obj, "mylist2")
  expect_that(mylist2.2, not(equals(mylist2)))
  expect_that(names(mylist2.2), equals(names(mylist2)))
  expect_that(mylist2.2,
              equals(lapply(obj$store$objects$get("mylist"), length)))

  ## Then, try and *rename* the source list; this should cause no
  ## reruns but should arrange for the output to be renamed.
  obj$store$objects$set_list_names("mylist", toupper(names(mylist)))
  mylist2.3 <- remake_make(obj, "mylist2")
  cmp <- mylist2.2
  names(cmp) <- toupper(names(cmp))
  expect_that(mylist2.3, equals(cmp))

  remake_make(obj, "notlist")
  expect_that(obj$store$objects$type("notlist"), equals("data"))

  remake_make(obj, "kindalist")
  expect_that(obj$store$objects$type("kindalist"), equals("list"))
})
