
# ---- test api functions ----

test_that(".ds_api returns correct url", {
  expect_equal(.ds_api(x), "https://irmaservices.nps.gov/datastore/v8/rest/")
})

test_that(".ds_secure_api returns correct url", {
  expect_equal(.ds_secure_api(x), "https://irmaservices.nps.gov/datastore-secure/v8/rest/")
})

test_that(".ds_dev_api returns correct url", {
  expect_equal(.ds_dev_api(x), "https://irmadevservices.nps.gov/datastore-secure/v8/rest/")
})

# ---- test get_user_input ----
test_that(".get_user_input request ouputs correct value (2)", {
  return_val_2 <- function() {2}
  local({mockr::local_mock(.get_user_input = return_val_2)
    var1 <- .get_user_input()
    
    expect_equal(var1, 2)
    })
})


test_that(".get_user_input request outputs correct value (1)", {
          return_val_1 <- function() {1}
          local({mockr::local_mock(.get_user_input = return_val_1)
            var1 <- .get_user_input()
            
            expect_equal(var1, 1)
            })
      })