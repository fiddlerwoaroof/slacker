BEGIN;

    CREATE TABLE slack_users (
	id UUID PRIMARY KEY DEFAULT uuid_generate_v1(),
	username TEXT,
	email TEXT,
	status TEXT,
	billing_active BOOL,
	has_2fa BOOL,
	has_sso BOOL,
	userid TEXT,
	fullname TEXT,
	displayname TEXT
    );

    CREATE INDEX slack_users_userid ON slack_users (userid);
    CREATE INDEX slack_users_email ON slack_users (email);

END;
