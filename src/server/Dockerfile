FROM python:3.7

## install fish, vim and nano
RUN yes | apt-get install fish
RUN yes | apt-get install nano
RUN yes | apt-get install vim

COPY requirements.txt ./
RUN pip install --no-cache-dir -r requirements.txt

COPY . .

ENV STATIC_URL /static
ENV STATIC_PATH /gisdata

CMD [ "python3", "./manage.py" ]


