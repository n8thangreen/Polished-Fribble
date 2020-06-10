BEGIN TRANSACTION;
CREATE TABLE IF NOT EXISTS "booking_table" (
	"booking_no"	INTEGER,
	"name"	TEXT,
	PRIMARY KEY("booking_no")
);
CREATE TABLE IF NOT EXISTS "calendar_table" (
	"booking_no"	INTEGER,
	"date"	TEXT,
	"time"	TEXT,
	"room_no"	TEXT,
	PRIMARY KEY("date","time","room_no")
);
CREATE TABLE IF NOT EXISTS "person_table" (
	"name"	TEXT,
	"room"	INTEGER,
	"password"	TEXT,
	"permissions"	TEXT,
	PRIMARY KEY("name")
);
INSERT INTO "booking_table" VALUES (1,'Nathan');
INSERT INTO "calendar_table" VALUES (1,'2020-01-01','9am_10am','1a');
INSERT INTO "calendar_table" VALUES (1,'2020-01-01','10am_11am','1a');
INSERT INTO "person_table" VALUES ('Nathan',1,'xxx','all');
COMMIT;
