CREATE TABLE public.time_series_p
(
    uuid uuid PRIMARY KEY,
    time_series uuid NOT NULL,
    time timestamp with time zone NOT NULL,
    p double precision NOT NULL
)
    WITHOUT OIDS
    TABLESPACE pg_default;

CREATE INDEX time_series_p_series_id ON time_series_p USING hash (time_series);

-- Order of columns is important when using btree: https://www.postgresql.org/docs/14/indexes-multicolumn.html
-- Column time_series needs to placed as the first argument since we at most use an equality constraint on
-- time_series and a range query on time.
CREATE UNIQUE INDEX time_series_p_series_time ON time_series_p USING btree (time_series, time);

INSERT INTO
    public.time_series_p (uuid, time_series, time, p)
VALUES
('0245d599-9a5c-4c32-9613-5b755fac8ca0', '9185b8c1-86ba-4a16-8dea-5ac898e8caa5', '2020-01-01 00:00:00+0', 1000.0),
('a5e27652-9024-4a93-9d2a-590fbc3ab5a1', '9185b8c1-86ba-4a16-8dea-5ac898e8caa5', '2020-01-01 00:15:00+0', 1250.0),
('b4a2b3e0-7215-431b-976e-d8b41c7bc71b', 'b669e4bf-a351-4067-860d-d5f224b62247', '2020-01-01 00:00:00+0', 50.0),
('1c8f072c-c833-47da-a3e9-5f4d305ab926', 'b669e4bf-a351-4067-860d-d5f224b62247', '2020-01-01 00:15:00+0', 100.0);
