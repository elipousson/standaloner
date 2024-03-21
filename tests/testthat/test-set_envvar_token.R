test_that("set_envvar_token works", {
  # FIXME: This test works manually but not when run with test()
  skip_on_ci()

  withr::local_envvar(
    c("TOKEN" = "test"),
    {
      expect_no_message(
        set_envvar_token(token = "testtoken", default = "TEST", quiet = TRUE)
      )

      expect_message(
        set_envvar_token(token = "testtoken", default = "TEST")
      )

      expect_error(
        set_envvar_token(token = "testoverwrite", default = "TOKEN", install = TRUE)
      )

      expect_identical(
        set_envvar_token(token = "testoverwrite", default = "TOKEN", install = FALSE, overwrite = TRUE),
        "testoverwrite"
      )
    }
  )
})

test_that("get_envvar_token works", {
  withr::with_envvar(
    c("TOKEN" = "test"),
    {
      expect_error(
        get_envvar_token(default = "TESTGETENVARTOKEN")
      )

      expect_identical(
        get_envvar_token(token = "token", default = "TESTGETENVARTOKEN"),
        "token"
      )

      expect_identical(
        get_envvar_token(default = "TOKEN"),
        "test"
      )
    }
  )
})
