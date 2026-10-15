;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :gdl-user)

;;
;; Anvil: the forge.  Repositories over Git's smart HTTP, a web face,
;; issues, merge requests, CI, a REST API in GitLab's shapes, OAuth2,
;; and the push-button deployment of a project into a hosting house.
;; ARCHITECTURE.md at the repository's root is the design.
;;

(gwl:define-package :anvil
  (:export #:initialize!
           #:start!
           #:stop!
           #:*data-directory*
           #:*hosts*
           #:*site-url*
           #:*site-name*
           #:*runner-tags*
           #:*runner-parallelism*
           #:*docker-socket*
           #:*deploy-houses*
           #:*push-mirrors?*
           #:*pull-mirror-interval*
           #:*pull-mirror-token*
           #:pull-mirrors!
           #:pull-mirror!
           #:set-pull-mirror!
           #:clear-pull-mirror!
           #:project-pull-mirror
           #:*read-only?*
           #:*read-only-text*
           #:read-only-environment?
           #:*registration-open?*
           #:make-user!
           #:make-group!
           #:make-project!
           #:find-user
           #:find-project
           #:run-pipeline!
           #:import-from-gitlab!
           #:import-gitlab-group!
           #:import-gitlab-project!
           #:make-gitlab-client
           #:fetch-repository!
           #:http-git-transport
           #:anvil-site))
