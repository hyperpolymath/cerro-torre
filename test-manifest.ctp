# Cerro Torre Bundle v0.2
# Manifest digest: sha256:test123
# Registry: localhost:5000
# Repository: test/myapp
# Reference: v1

# Manifest JSON:
{
  "schemaVersion": 2,
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "config": {
    "mediaType": "application/vnd.oci.image.config.v1+json",
    "size": 1024,
    "digest": "sha256:abcd1234"
  },
  "layers": [
    {
      "mediaType": "application/vnd.oci.image.layer.v1.tar+gzip",
      "size": 2048,
      "digest": "sha256:layer1"
    }
  ]
}
