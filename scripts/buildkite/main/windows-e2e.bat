

cd test\e2e
rmdir state\configs\preprod
md state\node_db\preprod
aws s3 cp s3://hal-team/preprod-20250102.zstd ./preprod-20250102.tar.zst
tar xzf ./preprod-20250102.tar.zst
mv db\* state\node_db\preprod\
bundle exec rake setup[preprod,%BUILDKITE_BRANCH%]
bundle exec rake display_versions
bundle exec rake start_node_and_wallet[preprod]
bundle exec rake wait_until_node_synced
bundle exec rake spec SPEC_OPTS="-e 'Stake Pools'"
bundle exec rake stop_node_and_wallet[preprod]