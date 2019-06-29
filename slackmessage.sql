BEGIN;

	CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

	DROP TABLE IF EXISTS slack_messages_raw CASCADE;
	CREATE TABLE slack_messages_raw
	(
		id UUID PRIMARY KEY DEFAULT uuid_generate_v1(),
		message JSONB,
		message_text text,
		slack_ts NUMERIC(32, 6)
	);

	CREATE OR REPLACE FUNCTION insert_messages_slack_ts() RETURNS TRIGGER AS $$
	BEGIN
		NEW.slack_ts := NEW.message->>'ts';
		IF not NEW.message ? 'hidden' OR NEW.message->'hidden' <> to_jsonb(true) THEN
			NEW.message_text := NEW.message #>> '{text}';
		END IF;
		RETURN NEW;
	END;
	$$ LANGUAGE PLPGSQL;

	CREATE TRIGGER trig_insert_messages_slack_ts
	BEFORE INSERT ON slack_messages_raw
	FOR EACH ROW EXECUTE PROCEDURE insert_messages_slack_ts();

COMMIT;
