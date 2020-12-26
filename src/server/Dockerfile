FROM python:3.7

COPY requirements.txt ./
RUN pip install --no-cache-dir -r requirements.txt

COPY . .

ENV STATIC_URL /static
ENV STATIC_PATH /gisdata

CMD [ "python3", "./manage.py" ]
