#include <SPI.h>
#include <Ethernet.h>
#include<stdlib.h>

//Sensors location

const int temperaturePin = A0;
const int brightnessPin = A1;
const float baselineTemp = 20.0;

//Brightness baseline
int brightnessValue;
int brightnessLow = 1023;
int brightnessHigh = 0;

//Ethernet setup
byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED };
byte server[] = { 10, 2, 20, 43 };
EthernetClient client;

void setup() {
  Serial.begin(57600);
  Ethernet.begin(mac);

  Serial.println("connecting...");

  //Connect to tcp server
  if (client.connect(server, 5678)) {
    Serial.println("connected");
  } else {
    Serial.println("connection failed");
  }

  //Calibrate brightness sensor
  while (millis() < 2000) {
    brightnessValue = analogRead(brightnessPin);

    if (brightnessValue > brightnessHigh) {
      brightnessHigh = brightnessValue;
    }
    if (brightnessValue < brightnessLow) {
      brightnessLow = brightnessValue;
    }
  }
}

void loop() {
  int temperatureValue = analogRead(temperaturePin);
  brightnessValue = analogRead(brightnessPin);

  float voltage = (temperatureValue/1024.0) * 5.0;
  float temperature = (voltage - 0.5) * 100;
  char tmp[4];

  String temperatureStr = dtostrf(temperature,1,2,tmp);
  String brightnessStr = String(brightnessValue);

  String dataPoint = "temp:f:" + temperatureStr + "|bright:i:" + brightnessStr;

  //Data sent as an array to avoid incomplete packets  
  int n = dataPoint.length()+1;
  char st[n];
  dataPoint.toCharArray(st,n);
  client.print(st);

  //Send data every 5 minutes
  delay(3000000);
}
