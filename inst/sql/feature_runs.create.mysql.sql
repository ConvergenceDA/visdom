-- The SQL standard does not support several fundamental features, so table
-- definitions need to be dialect-specific. This is the MySQL version of a table
-- definition for feature run metadata.
CREATE TABLE feature_runs (
	-- AUTO_INCREMENT isn't part of the SQL standard. 
	-- http://stackoverflow.com/questions/5823912/considering-the-different-implimentations-of-sequence-auto-increment-how-can-i
	-- mysql needs AUTO_INCREMENT
	-- psql needs the SERIAL datatype
	-- SQL Server needs IDENTITY(1,1) isntead of AUTO_INCREMENT
	-- Access needs AUTOINCREMENT
	-- Oracle needs a separate create sequence statement and a trigger: http://stackoverflow.com/questions/11296361/how-to-create-id-with-auto-increment-on-oracle
	id INT NOT NULL PRIMARY KEY AUTO_INCREMENT, 
	feature_set VARCHAR(20) NOT NULL UNIQUE,
	feature_set_description VARCHAR(250) NULL,
	run_name VARCHAR(40) NOT NULL UNIQUE,
	run_description VARCHAR(250) NULL,
	-- This only works with MySQL version 5.6.5 and above. See http://dev.mysql.com/doc/refman/5.6/en/timestamp-initialization.html
	create_time DATETIME NOT NULL,
	update_time DATETIME DEFAULT NULL
		COMMENT 'To be updated programmatically when features are written to the feature table.)', -- limit 255 chars
	CONSTRAINT feature_run UNIQUE (feature_set, run_name)
)
COMMENT 'Tracking metadata for feature sets and runs.' -- limit 60 chars
;
