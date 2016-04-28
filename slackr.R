library(slackr)

# slackr_setup : initialize necessary environment variables
# slackr : send stuff to Slack
# slackr_bot : send stuff to Slack using an incoming webhook URL
# dev_slackr : send the graphics contents of the current device to a to Slack channel
# ggslackr : send a ggplot object to a Slack channel (no existing device plot required, useful for scripts)
# save_slackr : save R objects to an RData file on Slack
# slackr_upload : upload any file to Slack
# slackr_users : get a data frame of Slack
# slackr_channels : get a data frame of Slack
# slackr_groups : get a data frame of Slack groups
# text_slackr : Send regular or preformatted messages to Slack
# slackr_msg : Slightly different version of text_slackr()


#API
#xoxp-36279594467-36329999717-38732390178-b098955541
slackrSetup(channel="#files", incoming_webhook_url="https://hooks.slack.com/services/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", api_token ="xoxp-36279594467-36329999717-38732390178-b098955541" )
#dev.slackr("#files")
slackr_upload("RH_Train_TN_Data")
