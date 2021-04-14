# LR-Randomized-Experiment-on-High-Note
Adopted Logistic Regression to help High Note to convert "free" to "fee".

## Background
High Note is an anonymized real music streaming company, similar to Last.fm, Spotify or Pandora, that uses a freemium business model.The “freemium” business model, widely used by online services such as LinkedIn, Match.com, Dropbox, and music-listening sites, divides user populations into groups that use the service for free and groups that pay a fee for additional features. Key points related to the freemium model:
  1.Free accounts are monetized using online advertising
  2.Premium subscribers (those paying a fee) are typically 24 times more profitable than free users
  3.premium subscribers are rare

Given the higher profitability of premium subscribers, it is generally in the interest of company to motivate users to go from “free to fee”; that is, convert free accounts to premium subscribers. Your task in regards to this case is to analyze the data for potential insight to inform a “free-to-fee” strategy.

## Summary statistics
After comparing two groups of adopter and non-adopter, we can see some points:
  1) the size of non-adopter observations in the data has a higher proportion (40300 ~ 92%) than that of the adopter observations (3527 ~ 8%).
  2) in terms of skew, we can find that there is a higher skewness value which is more than the value of +-2 in several variables: friend_cnt, friend_country_cnt, subscriber_friend_cnt, songsListened, lovedTracks, posts, playlists and shouts in both adopter and non-adopter subsamples.
  3) after t-test, we find that there is a significant difference between adopter and non-adopter groups with regard to the mean value of all variables since the p values are less than 0.01.
