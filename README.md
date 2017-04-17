# rumuki-server

## Endpoints

### `/api/device`

#### `POST`

##### Request

```javascript
{ deviceToken:  <string>
, keyFingerprint: <base64>
, apnDeviceToken: <base64?>
}
```

##### Response

```http
HTTP/1.1 201 Created
```

#### `DELETE`

##### Request

```javascript
{ deviceToken: <string>
, recordingUIDs: <[]string>
}
```

##### Response

```http
HTTP/1.1 204 No Content
```

------

### `/api/recording/:recordingUID/playback-grant`

#### `POST`

##### Request

```javascript
{ keyCipher: <base64>
, recipientKeyFingerprint: <base64>
, keyOffset: <int?>
}
```

##### Response

```http
HTTP/1.1 201 Created
Content-Type: application/json
```

```javascript
{ playbackGrant:
    { id: <int>
    , recordingUID: <string>
    , recipientKeyFingerprint: <base64>
    , expires: <time>
    , created: <time>
    }
}
```

#### `GET`

##### Response

```http
HTTP/1.1 200 OK
Content-Type: application/json
```

```javascript
{ playbackGrants:
    [{ id: <int>
     , recordingUID: <string>
     , recipientKeyFingerprint: <base64>
     , expires: <time>
     , created: <time>
    }]
}
```

#### `DELETE`

##### Response

```http
HTTP/1.1 204 No Content
```

------

### `/api/device/update`

#### `POST`

##### Request

```javascript
{ recordingUIDs: [<string>]
, deviceKeyFingerprint: <base64>
, longDistanceTransfers:
    [{ recordingUID: <string>
     , recordingNameCipher: <base64>
     , senderKeyFingerprintCipher: <base64>
     , keyCipher: <base64>
     , created: <time>
    }]
}
```

##### Response

```javascript
{ playbackGrants:
    [{ id: <int>
     , recordingUID: <string>
     , recipientKeyFingerprint: <base64>
     , expires: <time>
     , created: <time>
    }]
, screenCaptureDetections:
    [{ affectedDeviceKeyFingerprint: <base64>
    ,  recordingUID: <string>
    }]
}
```

------

### `/api/recording/:recordingUID/playback-grant/:playbackGrantID`

#### `GET`

##### Response

```http
HTTP/1.1 200 OK
Content-Type: application/json
```

```javascript
{ playbackGrant:
    { id: <int>
    , recordingUID: <string>
    , recipientKeyFingerprint: <base64>
    , expires: <time>
    , created: <time>
    , keyCipher: <base64>
    , keyOffset: <int?>
    }
}
```

------

### `/api/long-distance-transfer`

#### POST

##### Request

```javascript
{ recordingUID: <string>
, recipientKeyFingerprint: <base64>
, recordingNameCipher: <base64>
, senderKeyFingerprintCipher: <base64>
, keyCipher: <base64>
}
```

##### Response

```http
HTTP/1.1 200 OK
```

```javascript
{ uploadURL: <string>
}
```

------

### `/api/long-distance-transfer/:recordingUID`

#### GET

##### Response

```javascript
{ longDistanceTransfer:
   { recordingUID: <string>
   , recordingNameCipher: <base64>
   , senderKeyFingerprintCipher: <base64>
   , keyCipher: <base64>
   , created: <time>
   ]
, downloadURL: <string>
}
```

#### DELETE

##### Response

```http
HTTP/1.1 204 No Content
```

------

### `/api/subscriber`

#### `POST`

##### Request

```javascript
{ email: <string>
}
```

##### Response

```http
HTTP/1.1 201 Created
```

```javascript
{ subscriber:
    { email: <string>
    , created: <time>
    }
}
```

------

### `/api/feedback`

#### POST

##### Request

```javascript
{ email: <string?>
, content: <string>
}
```

##### Response

```http
HTTP/1.1 201 Created
```

```javascript
{ feedback:
    { email: <string?>
    , content: <string>
    , created: <time>
    }
}
```

------

### `/api/screen-capture-detection`

#### POST

##### Request

```javascript
{ recordingUID: <string>
, recipientKeyFingerprint: <base64>
}
```

##### Response

```http
HTTP/1.1 201 Created
```
