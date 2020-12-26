import psycopg2
import os

class PostgisConnector:

    def __init__(self):
        self.host = 'acmt-network_db_1'
        self.user = os.environ['POSTGRES_USER']
        self.pwd = os.environ['POSTGRES_PASSWORD']
        self.db = os.environ['POSTGRES_DB']

    def __get_connection(self):
        conn = psycopg2.connect(host=self.host,
                                database=self.db,
                                user=self.user,
                                password=self.pwd)
        conn.set_session(autocommit=True)
        return conn

    def get_lat_long(self, var_address):

        with self.__get_connection() as conn:
            with conn.cursor() as cursor:
                cursor.execute("""
                    SELECT 
                      g.rating
                    , ST_AsText(ST_SnapToGrid(g.geomout,0.00001)) As wktlonlat
                    , (addy).address As stno
                    , (addy).streetname As street
                    , (addy).streettypeabbrev As styp
                    , (addy).location As city
                    , (addy).stateabbrev As st
                    , (addy).zip
                    FROM geocode(%(var_address)s) As g;
                """, {
                    'var_address': var_address
                })
                result = cursor.fetchone()

        if result is None:
            return None

        # Remove text and split into len-2 array
        latlong = result[1].replace('POINT(','').replace(')','',).split(' ')

        output = {
            'lat': float(latlong[1]),
            'long': float(latlong[0]),
            'building': result[2],
            'street': result[3],
            'streetType': result[4],
            'city': result[5],
            'state': result[6],
            'zip': result[7]
        }

        return output