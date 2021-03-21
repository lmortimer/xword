npm run build
cd dist && aws s3 sync --delete --acl public-read . s3://static.isthisit.nz/xword
