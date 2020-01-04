add_package_checks()

if (Sys.getenv("id_rsa") != "" && inherits(ci(), "TravisCI")) {
  # pkgdown documentation can be built optionally. Other example criteria:
  # - `inherits(ci(), "TravisCI")`: Only for Travis CI
  # - `ci()$is_tag()`: Only for tags, not for branches
  # - `Sys.getenv("BUILD_PKGDOWN") != ""`: If the env var "BUILD_PKGDOWN" is set
  # - `Sys.getenv("TRAVIS_EVENT_TYPE") == "cron"`: Only for Travis cron jobs
  get_stage("before_deploy") %>%
    add_step(step_setup_ssh())

  get_stage("deploy") %>%
    add_step(step_build_pkgdown()) %>%
    add_code_step(zip("pkgdown.zip", "docs")) %>%
    add_code_step(
      print(
        paste(
          'curl -H "Content-Type: application/zip"',
          '-H "Authorization: Bearer $NETLIFY_KEY_PKG"',
          '--data-binary "@pkgdown.zip"',
          'https://api.netlify.com/api/v1/sites/$NETLIFY_SITEID_PKG/deploys'
        )
      )
    ) %>%
    add_code_step(
      system(
        paste(
          'curl -H "Content-Type: application/zip"',
          '-H "Authorization: Bearer $NETLIFY_KEY_PKG"',
          '--data-binary "@pkgdown.zip"',
          'https://api.netlify.com/api/v1/sites/$NETLIFY_SITEID_PKG/deploys'
        )
      )
    )
}

