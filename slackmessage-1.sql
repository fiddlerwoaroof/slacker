BEGIN;
	ALTER TABLE slack_messages_raw
	ADD COLUMN slack_channel text,
	ADD COLUMN slack_user text;

	UPDATE slack_messages_raw SET slack_channel = message->>'channel';
	UPDATE slack_messages_raw SET slack_user = message->>'user';

	CREATE OR REPLACE FUNCTION insert_messages_slack_ts() RETURNS TRIGGER AS $$
	BEGIN
		NEW.slack_ts := NEW.message->>'ts';
		NEW.slack_channel := NEW.message->>'channel';
		NEW.slack_user := NEW.message->>'user';

		IF not NEW.message ? 'hidden' OR NEW.message->'hidden' <> to_jsonb(true) THEN
			NEW.message_text := NEW.message #>> '{text}';
		END IF;
		RETURN NEW;
	END;
	$$ LANGUAGE PLPGSQL;
