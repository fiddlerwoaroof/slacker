BEGIN;
    CREATE INDEX slack_messages_raw_slack_ts_asc
      ON slack_messages_raw (slack_ts ASC NULLS LAST);

    CREATE INDEX slack_messages_raw_slack_ts_desc
      ON slack_messages_raw (slack_ts DESC NULLS LAST);

    CREATE INDEX slack_messages_raw_slack_channel_slack_ts_asc
      ON slack_messages_raw (slack_channel, slack_ts ASC NULLS LAST);

    CREATE INDEX slack_messages_raw_slack_user_slack_ts_asc
      ON slack_messages_raw (slack_user, slack_ts ASC NULLS LAST);

COMMIT;
