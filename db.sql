CREATE FUNCTION article_fulltext_trigger() RETURNS trigger AS $$
begin
  new.fulltext :=
     setweight(to_tsvector('pg_catalog.english', coalesce(new.title,'')), 'A') ||
     setweight(to_tsvector('pg_catalog.english', coalesce(new.content,'')), 'D');
  return new;
end
$$ LANGUAGE plpgsql;

CREATE TRIGGER tsvectorupdate BEFORE INSERT OR UPDATE
ON article FOR EACH ROW EXECUTE PROCEDURE article_fulltext_trigger();

CREATE INDEX article_fulltext_idx ON article USING gin(fulltext);
