FROM python:3.12

# install software
RUN apt-get update 
RUN apt-get install -y --allow-unauthenticated fish vim nano


COPY requirements.txt ./
RUN pip install --no-cache-dir -r requirements.txt

COPY . .

ENV STATIC_URL /static
ENV STATIC_PATH /gisdata

CMD [ "python3", "./manage.py" ]


