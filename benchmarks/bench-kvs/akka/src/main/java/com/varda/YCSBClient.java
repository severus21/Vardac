package com.varda;

//#cloc-exclude

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.io.StringWriter;
import java.io.Writer;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.Vector;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import site.ycsb.ByteIterator;
import site.ycsb.DB;
import site.ycsb.DBException;
import site.ycsb.Status;
import site.ycsb.StringByteIterator;

/**
 * A class that wraps the CouchbaseClient to allow it to be interfaced with YCSB.
 * This class extends {@link DB} and implements the database interface used by YCSB client.
 */
public class YCSBClient extends DB {
    public static final String URL_PROPERTY = "gateway.url";
    public static final String PORT_PROPERTY = "gateway.port";

    protected static final ObjectMapper JSON_MAPPER = new ObjectMapper();

    private MaingRPCClient client;
    private final Logger log = LoggerFactory.getLogger(getClass());



    @Override
    public void init() throws DBException {
        Properties props = getProperties();

        String ip = props.getProperty(URL_PROPERTY, "127.0.0.1");
        String port = props.getProperty(PORT_PROPERTY, "8090");

        try {
            client = new MaingRPCClient(ip, Integer.parseInt(port));
        } catch (Exception e) {
            throw new DBException("Could not create KVProtoServiceClient object.", e);
        }

        try{
            Thread.sleep(5000);
        }catch(InterruptedException e) {}
    }

    /**
     * Shutdown the client.
     */
    @Override
    public void cleanup() {
        //TODO
    }

    @Override
    public Status read(final String table, final String key, final Set<String> fields,
                        final Map<String, ByteIterator> result) {
        try {
            String value = client.get(key);
            decode(value, fields, result);
            return result.isEmpty() ? Status.ERROR : Status.OK;
        } catch (Exception e) {
            if (log.isErrorEnabled()) {
                log.error("Could not insert value for key " + key, e);
            }
            return Status.ERROR;
        }
    }

    @Override
    public Status scan(final String table, final String startkey, final int recordcount, final Set<String> fields,
                        final Vector<HashMap<String, ByteIterator>> result) {
        //Range scan are not supported
        return Status.ERROR;
    }

    @Override
    public Status update(final String table, final String key, final Map<String, ByteIterator> values) {
        return this.insert(table, key, values); 
    }

    @Override
    public Status insert(final String table, final String key, final Map<String, ByteIterator> values) {
        try {
            final Boolean flag = client.put(key, encode(values));
            return flag ? Status.OK : Status.ERROR;
        } catch (Exception e) {
            System.out.println("Error inserting "+e);
            if (log.isErrorEnabled()) {
                log.error("Could not insert value for key " + key, e);
            }
            return Status.ERROR;
        }
    }

    @Override
    public Status delete(final String table, final String key) {
        try {
            final Boolean future = client.delete(key);
            return Status.OK;
        } catch (Exception e) {
            if (log.isErrorEnabled()) {
                log.error("Could not delete value for key " + key, e);
            }
            return Status.ERROR;
        }
    }

    public static String keyof(String key, String field_name){
        return key + "_" + field_name;
    }

  /**
   * Decode the object from server into the storable result.
   *
   * @param source the loaded object.
   * @param fields the fields to check.
   * @param dest the result passed back to the ycsb core.
   */
    private void decode(final String source, final Set<String> fields, final Map<String, ByteIterator> dest) {
        try {
            JsonNode json = JSON_MAPPER.readTree(source);
            boolean checkFields = fields != null && !fields.isEmpty();
            for (Iterator<Map.Entry<String, JsonNode>> jsonFields = json.fields(); jsonFields.hasNext();) {
            Map.Entry<String, JsonNode> jsonField = jsonFields.next();
            String name = jsonField.getKey();
            if (checkFields && fields.contains(name)) {
                continue;
            }
            JsonNode jsonValue = jsonField.getValue();
            if (jsonValue != null && !jsonValue.isNull()) {
                dest.put(name, new StringByteIterator(jsonValue.asText()));
            }
            }
        } catch (Exception e) {
            throw new RuntimeException("Could not decode JSON");
        }
    }

  /**
   * Encode the object for couchbase storage.
   *
   * @param source the source value.
   * @return the storable object.
   */
  private String encode(final Map<String, ByteIterator> source) {
    Map<String, String> stringMap = StringByteIterator.getStringMap(source);

    ObjectNode node = JSON_MAPPER.createObjectNode();
    for (Map.Entry<String, String> pair : stringMap.entrySet()) {
      node.put(pair.getKey(), pair.getValue());
    }
    JsonFactory jsonFactory = new JsonFactory();
    Writer writer = new StringWriter();
    try {
      JsonGenerator jsonGenerator = jsonFactory.createGenerator(writer);
      JSON_MAPPER.writeTree(jsonGenerator, node);
    } catch (Exception e) {
      throw new RuntimeException("Could not encode JSON value");
    }
    return writer.toString();
  }
}