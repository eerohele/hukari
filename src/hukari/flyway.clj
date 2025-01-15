(ns hukari.flyway
  (:require [clojure.core.protocols :refer [Datafiable]]
            [clojure.datafy :as datafy])
  (:import (org.flywaydb.core.api.output MigrateOutput MigrateResult)))

(extend-protocol Datafiable
  MigrateOutput
  (datafy [this]
    {:category (.category this)
     :description (.description this)
     :execution-time (.executionTime this)
     :file-path (.filepath this)
     :type (.type this)
     :version (.version this)})

  MigrateResult
  (datafy [this]
    {:database (.database this)
     :flyway-version (.flywayVersion this)
     :warnings (vec (.warnings this))
     :initial-schema-version (.initialSchemaVersion this)
     :migrations (mapv datafy/datafy (.migrations this))
     :migrations-executed (.migrationsExecuted this)
     :schema-name (.schemaName this)
     :target-schema-version (.targetSchemaVersion this)}))

(defmethod print-method MigrateResult
  [x ^java.io.Writer w]
  (print-method (datafy/datafy x) w))
