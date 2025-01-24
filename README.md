> ⚠️ **WARNING:**
> This code is published as-is for reference and educational purposes in the field of deepfake detection. It represents a historical implementation by TrueMedia.org and is not actively maintained. The repository does not accept pull requests, issues, modifications, or support requests. The original TrueMedia.org organization has ceased operations.

# Media URL Resolver

This microservice accepts a social media post URL and attempts to extract any media URLs that are
contained therein. Those media URLs can then be sent along to deepfake detection partners or TrueMedia.org's own deepfake detection services to be checked, and the partners and services need not concern
themselves with extracting media URLs themselves.

This service was developed by [TrueMedia.org](https://www.truemedia.org/), a non-profit service that detects deepfakes in social media. The service is required by the [TrueMedia.org web application](https://github.com/truemediaorg/deepfake-app).

[![TrueMedia.org video](https://raw.githubusercontent.com/truemediaorg/.github/main/profile/video-splash.png)](https://www.youtube.com/watch?v=-6l7Jg02C8E)

Currently we support:

- TikTok
- X
- Instagram
- Facebook
- TruthSocial
- Reddit
- Mastodon
- BitChute.com
- And a wide variety of other sites supported by [yt-dlp](https://github.com/yt-dlp/yt-dlp)

We cannot support YouTube, since YouTube prevents third parties from downloading videos from their site.

## Initial setup

When setting up this project for the first time, you'll need to search the code for `OPEN-TODO-PLACEHOLDER` and replace with your specific service information.

## Developing

To build and test this service locally, use [SBT] and build and run it in the usual way. Additionally a few environment variables need to be set to run the service:

- `AWS_REGION` - The AWS region to use for the S3 bucket.
- `SECRETS` - A JSON object containing the AWS access key and secret key to use for the S3 bucket as well as any service specific api keys for twitter, fb etc...

```
AWS_REGION=us-east-2 SECRETS='{"AWS_ACCESS_KEY_ID": "YOUR_ID_HERE", "AWS_SECRET_ACCESS_KEY": "YOUR_KEY_HERE"}' sbt run
```

Or more likely, `sbt` and then `run` in the SBT console.

The Reddit backend uses a Reddit "app" and thus needs an app id and app secret. You can
create a simple Reddit app for testing.

## Deploying

The microservice is deployed to AWS as a Docker image pushed to an ECR
repository. To build the Docker image locally, run the `docker` task in SBT, or from the command
line:

```
sbt docker
```

This will create the image in your local Docker installation, which you can confirm via:

```
% docker image ls
REPOSITORY                                                 TAG       IMAGE ID       CREATED        SIZE
PLACEHOLDER.dkr.ecr.us-east-2.amazonaws.com/PLACEHOLDER    latest    f60a47025c47   4 hours ago    527MB
```

To push this to ECR, you must have the `aws-cli` tool installed and configured to use an IAM
account which has the necessary privileges, then run the following to obtain an authentication
token for Docker:

```
aws ecr get-login-password --region us-east-2 | \
  docker login --username AWS --password-stdin PLACEHOLDER.dkr.ecr.us-east-2.amazonaws.com
```

And then push the image to ECR like so:

```
docker push PLACEHOLDER.dkr.ecr.us-east-2.amazonaws.com/PLACEHOLDER:latest
```

## Licenses

This project is licensed under the terms of the MIT license.

## Original Contributors

This TrueMedia.org service was built by [Michael Bayne](https://github.com/samskivert), with contributions from [Alex Schokking](https://github.com/aschokking), [Michael Langan](https://github.com/mjlangan), and [Paul Carduner](https://github.com/pcardune).
