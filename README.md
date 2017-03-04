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
    }]
}
```

#### `DELETE`

##### Response

```http
HTTP/1.1 204 No Content
```

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
    , keyCipher: <base64>
    , keyOffset: <int?>
    }
}
```

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
