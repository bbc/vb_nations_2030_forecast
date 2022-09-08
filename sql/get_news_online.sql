BEGIN;
set search_path TO 'central_insights_sandbox';

/*DROP TABLE IF EXISTS vb_news_regions_historic_users;
CREATE TABLE vb_news_regions_historic_users
(
    week_commencing     date,
    page_producer       varchar(400),
    visitors_raw        bigint,
    requests_raw        bigint,
    visitors            bigint,
    visits_raw          bigint,
    request_per_visitor double precision
) DISTSTYLE AUTO;*/

GRANT ALL on vb_news_regions_historic_users to vicky_banks with grant option;
GRANT SELECT on vb_news_regions_historic_users to GROUP central_insights;

--- backfill table
INSERT INTO vb_news_regions_historic_users
SELECT '<params.run_date>'::date                                                              as week_commencing,
       CASE
           WHEN page_producer ILIKE '%English%' THEN 'England'
           WHEN page_producer ILIKE '%Wales%' THEN 'Wales'
           WHEN page_producer ILIKE '%Scotland%' THEN 'Scotland'
           WHEN page_producer ILIKE '%Northern Ireland%' THEN 'Northern Ireland' END as page_producer,
       count(distinct unique_visitor_cookie_id)                                      as visitors_raw,
       count(*)                                                                      as requests_raw,
        round(count(distinct unique_visitor_cookie_id), -4)                           as visitors,
       count(distinct visit_id)                                                      as visits_raw,
       round(requests_raw::double precision / visitors_raw, 1)                       as request_per_browser
FROM s3_audience.audience_activity
WHERE dt >= '<params.run_date>' AND dt < replace(cast('<params.run_date>'::varchar as date) + 7, '-', '')
  AND destination = 'PS_NEWS'
  AND source = 'Events'      -- set to the event scope
  AND content_action IS NULL -- remove any AV things
  AND (page_producer ILIKE '%Wales%' OR
       page_producer ILIKE '%Scotland%' OR
       page_producer ILIKE '%Ireland%' OR
       page_producer ILIKE '%English%')
  AND page_producer NOT ILIKE '%sport%'
  AND page_name != 'keepalive'
  AND app_type != 'web'      -- to exclude the unusual data from interactive maps
  AND page_producer != 'WS Learning English'
GROUP BY 1, 2
ORDER BY visitors_raw desc

;

END;

SELECT  week_commencing, page_producer, visitors FROM vb_news_regions_historic_users ORDER BY page_producer, week_commencing;
SELECT  week_commencing, page_producer, visitors, round(visits_raw,-4) as visits FROM vb_news_regions_historic_users ORDER BY page_producer, week_commencing;



SELECT DISTINCT week_commencing  FROM vb_news_regions_historic_users ORDER BY 1 DESC;